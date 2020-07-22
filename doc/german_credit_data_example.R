## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, echo = TRUE, warning = FALSE, message = FALSE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(tidymodels)
library(corels)
library(tidycorels)
library(funModeling)
library(pROC)
library(formattable)

## -----------------------------------------------------------------------------
clean_df <- read.table(file = "https://www.openml.org/data/get_csv/31/dataset_31_credit-g.arff",
                       header = TRUE,
                       sep = ",") %>%
  janitor::clean_names(., "small_camel")

## -----------------------------------------------------------------------------
set.seed(seed = 943)
training_split <-
  rsample::initial_split(
    data = clean_df,
    prop = 0.8
  )

trainData <- rsample::training(training_split)
testData <- rsample::testing(training_split)

set.seed(seed = 1738)
folds <- rsample::vfold_cv(trainData,
  v = 5
)

## -----------------------------------------------------------------------------
# https://blog.datascienceheroes.com/discretization-recursive-gain-ratio-maximization/
trainData_cat <-
  trainData %>%
  dplyr::mutate(dplyr:::across(
    .cols = c(duration, creditAmount,age),
    list(~ funModeling::discretize_rgr(input = ., target = class)),
    .names = "{col}Cat"
  ))

vars <- base::colnames(dplyr::select(trainData_cat, dplyr::contains("Cat")))

age_cat_cuts <-
  trainData_cat %>%
  group_by(ageCat) %>%
  dplyr::summarise(min = min(age), .groups = 'drop') %>%
  dplyr::pull(min)

duration_cat_cuts <-
  trainData_cat %>%
  group_by(durationCat) %>%
  dplyr::summarise(min = min(duration), .groups = 'drop') %>%
  dplyr::pull(min)

amount_cat_cuts <-
  trainData_cat %>%
  group_by(creditAmountCat) %>%
  dplyr::summarise(min = min(creditAmount), .groups = 'drop') %>%
  dplyr::pull(min)

## -----------------------------------------------------------------------------
plot_fun <- function(cat) {
  cat <- rlang::ensym(cat)

  trainData_cat %>%
    dplyr::group_by(!!cat, class) %>%
    dplyr::summarise(n = n(), .groups = 'drop') %>%
    ggplot2::ggplot() +
    ggplot2::aes(
      x = !!cat,
      y = n,
      fill = class,
      label = n
    ) +
    ggplot2::geom_bar(
      position = "fill",
      stat = "identity"
    ) +
    ggplot2::geom_text(
      size = 3,
      position = position_fill(vjust = 0.5),
      colour = "black"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.border = element_blank(),
      strip.text.x = element_text(size = 10),
      axis.text.x = element_text(
        # angle = 60,
        hjust = 1,
        size = 10
      ),
      legend.text = element_text(size = 12),
      legend.position = "right",
      legend.direction = "vertical",
      plot.title = element_text(
        size = 22,
        face = "bold"
      )
    )
}

## -----------------------------------------------------------------------------
cols <- base::colnames(dplyr::select(trainData_cat, -duration, -age, -creditAmount)) # getting all the column names but removing the continuous columns that have been binned.
cols <- cols[cols != "class"] # remove outcome column from list of column names
plots <- purrr::map(cols, plot_fun)
print(plots)

## -----------------------------------------------------------------------------
# create the data preperation recipe
credit_recipe <-
  recipes::recipe(class ~ .,
    data = trainData
  ) %>%
  # Apply the funModeling::discretize_rgr() cut points to continuous columns that maximise the information gain ratio on the training data only
  recipes::step_mutate(age = base::cut(age, breaks = c("-inf", age_cat_cuts, "inf"), right = FALSE, dig.lab = 10)) %>%
  recipes::step_mutate(duration = base::cut(duration, breaks = c("-inf", duration_cat_cuts, "inf"), right = FALSE, dig.lab = 10)) %>%
  recipes::step_mutate(creditAmount = base::cut(creditAmount, breaks = c("-inf", amount_cat_cuts, "inf"), right = FALSE, dig.lab = 10)) %>%
  # ensure column values that are words do not have spaces. This will form better dummy column names that tidycorels needs where the value each dummy column represents is shown by a single delimiter (e.g. the underscore step_dummy creates)
  recipes::step_mutate_at(recipes::all_predictors(), fn = list(~ base::gsub(pattern = "_", replacement = ".", x = .))) %>%
  recipes::step_mutate_at(recipes::all_predictors(), fn = list(~ as.factor(.))) %>%
  recipes::step_mutate(class = as.factor(class)) %>% # step_dummy requires variable values to be factors
  recipes::step_dummy(recipes::all_predictors(), one_hot = TRUE) %>%
  recipes::step_integer(recipes::all_outcomes(), zero_based = TRUE) %>% # ensure outcome is 0/1 rather than words
  recipes::step_mutate_at(recipes::all_outcomes(), fn = list(~ as.factor(.))) %>% # step_dummy requires variable values to be factors
  recipes::step_dummy(recipes::all_outcomes(), one_hot = TRUE)

# Train the data preperation recipe on the training data
credit_recipe_trained <- recipes::prep(credit_recipe, training = trainData)

# Apply the recipe that has been trained on the training data to both the training and test data
credit_train_preprocessed <- recipes::bake(credit_recipe_trained, new_data = trainData)
credit_test_preprocessed <- recipes::bake(credit_recipe_trained, new_data = testData)

## -----------------------------------------------------------------------------
credit_train_model <-
    tidycorels::tidy_corels(
      df = credit_train_preprocessed,
      label_cols = c("class_X0", "class_X1"),
      value_delim = "_",
      run_bfs = TRUE,
      calculate_size = TRUE,
      run_curiosity = TRUE,
      regularization = 0.01, 
      curiosity_policy = 3
    )

## -----------------------------------------------------------------------------
easyalluvial::alluvial_wide(credit_train_model$alluvial_df, verbose = TRUE) +
  ggplot2::coord_flip()

## -----------------------------------------------------------------------------
credit_test_predict <-
  tidycorels::predict_corels(
    model = credit_train_model,
    new_df = credit_test_preprocessed
  )

## -----------------------------------------------------------------------------
conf_matrix <-
  credit_test_predict$new_df_labelled %>%
  yardstick::conf_mat(
    truth = "class_X1",
    estimate = "corels_label"
  )

ggplot2::autoplot(conf_matrix, "heatmap")

# https://github.com/tidymodels/yardstick/issues/160
withr::with_options(c(yardstick.event_first = FALSE),summary(conf_matrix)) %>% 
  dplyr:::mutate(.estimate = round(.estimate, digits = 3)) %>%
  dplyr::select(.metric, .estimate) %>%
  dplyr::filter(.metric %in% c("accuracy","bal_accuracy", "mcc","precision", "recall", "f_meas")) %>%
  dplyr::mutate(.estimate = formattable::color_tile("white", "orange")(.estimate)) %>%
  kableExtra::kable(escape = F) %>%
  kableExtra::kable_styling("hover", full_width = F)

## -----------------------------------------------------------------------------
easyalluvial::alluvial_wide(credit_test_predict$alluvial_df, verbose = TRUE) +
  ggplot2::coord_flip() 

## -----------------------------------------------------------------------------
xgb_pre_proc <-
  recipes::recipe(class ~ .,
    data = trainData
  ) %>%
  recipes::step_dummy(recipes::all_nominal(),
    -recipes::all_outcomes(),
    one_hot = TRUE
  )

## -----------------------------------------------------------------------------
xgb_mod <-
  parsnip::boost_tree(
    mtry = tune::tune(),
    trees = 500,
    min_n = tune::tune(),
    tree_depth = tune::tune(),
    learn_rate = 0.01,
    sample_size = tune::tune()
  ) %>%
  parsnip::set_mode("classification") %>%
  parsnip::set_engine("xgboost")

## -----------------------------------------------------------------------------
xgb_wflow <-
  workflows::workflow() %>%
  workflows::add_recipe(xgb_pre_proc) %>%
  workflows::add_model(xgb_mod)

## -----------------------------------------------------------------------------
xgb_wflow %>%
  dials::parameters()

## -----------------------------------------------------------------------------
feat_count <- ncol(xgb_pre_proc %>%
  recipes::prep() %>%
  recipes::juice() %>%
  dplyr::select(-class))

mtry_min <- base::floor(feat_count * 0.4)

xgb_params <-
  xgb_wflow %>%
  dials::parameters() %>%
  stats::update(
    mtry = dials::mtry(range = c(mtry_min, feat_count)),
    sample_size = dials::sample_prop(c(0.5, 1)),
    tree_depth = dials::tree_depth(range = c(4L, 10L))
  )

## -----------------------------------------------------------------------------
set.seed(1645)

xgb_grid <-
  xgb_params %>%
  dials::grid_max_entropy(size = 10) %>%
  dplyr::mutate(sample_size = as.double(sample_size))

xgb_grid

## -----------------------------------------------------------------------------
tune_xgb_grid <-
  tune::tune_grid(xgb_wflow,
    resamples = folds,
    grid = xgb_grid,
    param_info = xgb_params,
    metrics = yardstick::metric_set(roc_auc),
    control = tune::control_grid(
      verbose = TRUE,
      save_pred = TRUE
    )
  )

tune_xgb_grid

## -----------------------------------------------------------------------------
set.seed(1600)

xgb_bayes_tune <-
  tune::tune_bayes(xgb_wflow,
    resamples = folds,
    iter = 5L,
    param_info = xgb_params,
    metrics = yardstick::metric_set(roc_auc),
    initial = tune_xgb_grid,
    control = tune::control_bayes(
      verbose = TRUE,
      save_pred = TRUE,
      no_improve = 5L
    )
  )

## ----best xgb hyperparams-----------------------------------------------------
best_xgb <- 
  xgb_bayes_tune %>%
  tune::select_best(metric = "roc_auc")

best_xgb

## ----finalising a workflow----------------------------------------------------
best_xgb_wfl <-
  tune::finalize_workflow(xgb_wflow,
    parameters = best_xgb
  )
best_xgb_wfl

## ----final fit for XGBoost----------------------------------------------------
final_fit <-
  best_xgb_wfl %>%
  parsnip::fit(data = trainData)

final_fit

## ----var imp plot XGBoost-----------------------------------------------------
final_fit %>%
  workflows::pull_workflow_fit() %>%
  vip::vip(
    geom = "col",
    num_features = 12L,
    include_type = TRUE
  )

## -----------------------------------------------------------------------------
xgb_train_preds <- final_fit %>%
  stats::predict(
    new_data = trainData,
    type = "prob"
  ) %>%
  dplyr::bind_cols(trainData %>% dplyr::select(class))

# Generate the full ROC curve
xgb_train_preds$class <- as.factor(xgb_train_preds$class)
xgb_roc_curve <- yardstick::roc_curve(xgb_train_preds,
  truth = class,
  .pred_good
)

# Get the AUC value and plot the curve
tune::autoplot(xgb_roc_curve) +
  labs(
    title = sprintf(
      "AUC for XGBoost model on train set: %.2f",
      yardstick::roc_auc(xgb_train_preds,
        truth = class,
        .pred_good
      ) %>%
        dplyr::pull(.estimate)
    )
  )


## -----------------------------------------------------------------------------
# calculate best threshold for classification label
my_roc <- pROC::roc(
  predictor = xgb_train_preds$.pred_good,
  response = xgb_train_preds$class
)
threshold <- pROC::coords(my_roc, "best", ret = "threshold", transpose = TRUE) %>%
  as.double()

xgb_train_preds <-
  xgb_train_preds %>%
  dplyr::mutate(.pred_class = dplyr::case_when(
    .pred_good >= threshold ~ "good",
    TRUE ~ "bad"
  )) %>%
  dplyr::mutate(.pred_class = as.factor(.pred_class))

# confusion matrix
cmat_train <- yardstick::conf_mat(xgb_train_preds,
  truth = "class",
  estimate = ".pred_class"
)

ggplot2::autoplot(cmat_train, "heatmap")

## -----------------------------------------------------------------------------
# https://github.com/tidymodels/yardstick/issues/160
withr::with_options(
  c(yardstick.event_first = FALSE),
  summary(cmat_train)
) %>% 
  dplyr:::mutate(.estimate = round(.estimate, digits = 3)) %>%
  dplyr::select(.metric, .estimate) %>%
  dplyr::filter(.metric %in% c("accuracy", "bal_accuracy","mcc","precision", "recall", "f_meas")) %>%
  dplyr::mutate(.estimate = formattable::color_tile("white", "orange")(.estimate)) %>%
  kableExtra::kable(escape = F) %>%
  kableExtra::kable_styling("hover", full_width = F)

## ----test AUC XGBoost---------------------------------------------------------
xgb_test_preds <- final_fit %>%
  stats::predict(
    new_data = testData,
    type = "prob"
  ) %>%
  dplyr::bind_cols(testData %>% dplyr::select(class))

xgb_test_preds <-
  xgb_test_preds %>%
  dplyr::mutate(.pred_class = dplyr::case_when(
    .pred_good >= threshold ~ "good",
    TRUE ~ "bad"
  )) %>%
  dplyr::mutate(.pred_class = as.factor(.pred_class))

# Generate the full ROC curve
xgb_test_preds$class <- as.factor(xgb_test_preds$class)
xgb_roc_curve <- yardstick::roc_curve(xgb_test_preds,
  truth = class,
  .pred_good
)

# Get the AUC value and plot the curve
tune::autoplot(xgb_roc_curve) +
  labs(
    title = sprintf(
      "AUC for XGBoost model on test set: %.2f",
      yardstick::roc_auc(xgb_test_preds,
        truth = class,
        .pred_good
      ) %>%
        dplyr::pull(.estimate)
    )
  )

# confusion matrix
cmat_test <- yardstick::conf_mat(xgb_test_preds,
  truth = "class",
  estimate = ".pred_class"
)

ggplot2::autoplot(cmat_test, "heatmap")

# https://github.com/tidymodels/yardstick/issues/160
withr::with_options(
  c(yardstick.event_first = FALSE),
  summary(cmat_test)
) %>% 
  dplyr:::mutate(.estimate = round(.estimate, digits = 3)) %>%
  dplyr::select(.metric, .estimate) %>%
  dplyr::filter(.metric %in% c("accuracy", "bal_accuracy","mcc","precision", "recall", "f_meas")) %>%
  dplyr::mutate(.estimate = formattable::color_tile("white", "orange")(.estimate)) %>%
  kableExtra::kable(escape = F) %>%
  kableExtra::kable_styling("hover", full_width = F)

