tidycorels
================

  - [What is corels and tidycorels?](#what-is-corels-and-tidycorels)
  - [Installation](#installation)
  - [An example](#an-example)
  - [Prepare dataframe for Corels](#prepare-dataframe-for-corels)
  - [Run tidycorels](#run-tidycorels)
  - [Performance on test data](#performance-on-test-data)
  - [Alluvial plot](#alluvial-plot)

<img src="./images/train_alluvial.svg" width="80%" />

## What is corels and tidycorels?

> Corels are [‘Certifiably Optimal RulE
> ListS’](https://corels.eecs.harvard.edu/). They are short and simple
> human [interpretable rule lists](https://arxiv.org/pdf/1704.01701.pdf)
> created on categorical data.

`tidycorels::tidy_corels()` converts your dataframe into two text files
in the format that the R package
[corels](https://cran.r-project.org/package=corels) expects. It returns
the Corels rules converted to `data.table` code and applies them to your
dataframe.

Another dataframe is returned that includes only the true label, the
columns used in the rules, and the classification by corels. This
dataframe is created to be used in an insightful
[alluvial](https://github.com/erblast/easyalluvial/blob/master/README.md)
plot showing you visually how the rules are applied.

`tidycorels::predict_corels()` applies the `data.table` rules to a new
dataframe (e.g. test data). It also returns a data frame that works well
with an
[alluvial](https://github.com/erblast/easyalluvial/blob/master/README.md)
plot letting you inspect the correct and incorrect classificiations
visually and intuitively.

## Installation

``` r
devtools::install_github("billster45/tidycorels")
```

## An example

``` r
library(tidymodels)
library(corels)
library(tidycorels)
library(kableExtra)
library(easyalluvial)
library(parcats)
library(formattable)

kable_table <- function(table, title) {
  kableExtra::kable(table, caption = title) %>%
    kableExtra::kable_styling(
      latex_options = "hold_position",
      full_width = F,
      bootstrap_options = c("striped", "condensed"),
      position = "left"
    )
}
```

In this example, we re-use the exact `recipes` data preperation steps
from the excellent tidymodels walkthrough by [Rebecca
Barter](http://www.rebeccabarter.com/blog/2020-03-25_machine_learning/)

``` r
# load the Pima Indians dataset from the mlbench dataset
library(mlbench)
data(PimaIndiansDiabetes)
diabetes_orig <- PimaIndiansDiabetes

diabetes_clean <- diabetes_orig %>%
  dplyr::mutate_at(
    vars(triceps, glucose, pressure, insulin, mass),
    function(.var) {
      if_else(condition = (.var == 0), # if true (i.e. the entry is 0)
        true = as.numeric(NA), # replace the value with NA
        false = .var # otherwise leave it as it is
      )
    }
  )

kable_table(head(diabetes_clean, 5), title = "Pima Indians Dataset")
```

<table class="table table-striped table-condensed" style="width: auto !important; ">

<caption>

Pima Indians Dataset

</caption>

<thead>

<tr>

<th style="text-align:right;">

pregnant

</th>

<th style="text-align:right;">

glucose

</th>

<th style="text-align:right;">

pressure

</th>

<th style="text-align:right;">

triceps

</th>

<th style="text-align:right;">

insulin

</th>

<th style="text-align:right;">

mass

</th>

<th style="text-align:right;">

pedigree

</th>

<th style="text-align:right;">

age

</th>

<th style="text-align:left;">

diabetes

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

148

</td>

<td style="text-align:right;">

72

</td>

<td style="text-align:right;">

35

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

33.6

</td>

<td style="text-align:right;">

0.627

</td>

<td style="text-align:right;">

50

</td>

<td style="text-align:left;">

pos

</td>

</tr>

<tr>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

85

</td>

<td style="text-align:right;">

66

</td>

<td style="text-align:right;">

29

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

26.6

</td>

<td style="text-align:right;">

0.351

</td>

<td style="text-align:right;">

31

</td>

<td style="text-align:left;">

neg

</td>

</tr>

<tr>

<td style="text-align:right;">

8

</td>

<td style="text-align:right;">

183

</td>

<td style="text-align:right;">

64

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

NA

</td>

<td style="text-align:right;">

23.3

</td>

<td style="text-align:right;">

0.672

</td>

<td style="text-align:right;">

32

</td>

<td style="text-align:left;">

pos

</td>

</tr>

<tr>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

89

</td>

<td style="text-align:right;">

66

</td>

<td style="text-align:right;">

23

</td>

<td style="text-align:right;">

94

</td>

<td style="text-align:right;">

28.1

</td>

<td style="text-align:right;">

0.167

</td>

<td style="text-align:right;">

21

</td>

<td style="text-align:left;">

neg

</td>

</tr>

<tr>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

137

</td>

<td style="text-align:right;">

40

</td>

<td style="text-align:right;">

35

</td>

<td style="text-align:right;">

168

</td>

<td style="text-align:right;">

43.1

</td>

<td style="text-align:right;">

2.288

</td>

<td style="text-align:right;">

33

</td>

<td style="text-align:left;">

pos

</td>

</tr>

</tbody>

</table>

The Pima Indians diabetes data set is split into train and test
datasets.

``` r
set.seed(234589)
diabetes_split <- rsample::initial_split(diabetes_clean,
  prop = 3 / 4
)

diabetes_train <- rsample::training(diabetes_split)
diabetes_test <- rsample::testing(diabetes_split)
```

## Prepare dataframe for Corels

We now apply the same `recipe` steps as Rebecca, but with the additional
step to
[discretise](https://recipes.tidymodels.org/reference/step_discretize.html)
the continous variables. Then each bin in each column is given its own
0/1 binary column using
[`recipes::step_dummy()`](https://recipes.tidymodels.org/reference/step_dummy.html).
This is sometimes called one-hot encoding.

Finally, Corels requires the label column `diabetes` is split into two
columns representing each class. We do this by first ensuring the values
are 0 and 1, then using using `recipes::step_integer()` followed by
[`recipes::step_dummy()`](https://recipes.tidymodels.org/reference/step_dummy.html)
to create the two label columns.

``` r
diabetes_recipe <-
  recipes::recipe(diabetes ~ pregnant + glucose + pressure + triceps +
    insulin + mass + pedigree + age,
  data = diabetes_clean
  ) %>%
  recipes::step_normalize(all_numeric()) %>%
  recipes::step_knnimpute(all_predictors()) %>%
  # discretise numeric variables into bins
  recipes::step_discretize(pregnant, glucose, pressure, triceps, insulin, mass, pedigree, age, min_unique = 1) %>%
  recipes::step_mutate_at(recipes::all_predictors(), fn = list(~ as.factor(.))) %>%
  recipes::step_mutate_at(recipes::all_outcomes(), fn = list(~ as.factor(.))) %>%
  recipes::step_dummy(recipes::all_predictors(), one_hot = TRUE) %>% # one-hot encode all discretised predictors
  recipes::step_nzv(recipes::all_predictors()) %>%
  recipes::step_integer(recipes::all_outcomes(), zero_based = TRUE) %>% # ensure outcome is 0/1 rather than words
  recipes::step_mutate_at(recipes::all_outcomes(), fn = list(~ as.factor(.))) %>%
  recipes::step_dummy(recipes::all_outcomes(), one_hot = TRUE)

diabetes_train_preprocessed <-
  diabetes_recipe %>%
  # apply the recipe to the training data
  recipes::prep(diabetes_train) %>%
  # extract the pre-processed training dataset
  recipes::juice()

kable_table(head(diabetes_train_preprocessed, 5), title = "diabetes data - juiced")
```

<table class="table table-striped table-condensed" style="width: auto !important; ">

<caption>

diabetes data - juiced

</caption>

<thead>

<tr>

<th style="text-align:right;">

pregnant\_bin1

</th>

<th style="text-align:right;">

pregnant\_bin2

</th>

<th style="text-align:right;">

pregnant\_bin3

</th>

<th style="text-align:right;">

pregnant\_bin4

</th>

<th style="text-align:right;">

glucose\_bin1

</th>

<th style="text-align:right;">

glucose\_bin2

</th>

<th style="text-align:right;">

glucose\_bin3

</th>

<th style="text-align:right;">

glucose\_bin4

</th>

<th style="text-align:right;">

pressure\_bin1

</th>

<th style="text-align:right;">

pressure\_bin2

</th>

<th style="text-align:right;">

pressure\_bin3

</th>

<th style="text-align:right;">

pressure\_bin4

</th>

<th style="text-align:right;">

triceps\_bin1

</th>

<th style="text-align:right;">

triceps\_bin2

</th>

<th style="text-align:right;">

triceps\_bin3

</th>

<th style="text-align:right;">

triceps\_bin4

</th>

<th style="text-align:right;">

insulin\_bin1

</th>

<th style="text-align:right;">

insulin\_bin2

</th>

<th style="text-align:right;">

insulin\_bin3

</th>

<th style="text-align:right;">

insulin\_bin4

</th>

<th style="text-align:right;">

mass\_bin1

</th>

<th style="text-align:right;">

mass\_bin2

</th>

<th style="text-align:right;">

mass\_bin3

</th>

<th style="text-align:right;">

mass\_bin4

</th>

<th style="text-align:right;">

pedigree\_bin1

</th>

<th style="text-align:right;">

pedigree\_bin2

</th>

<th style="text-align:right;">

pedigree\_bin3

</th>

<th style="text-align:right;">

pedigree\_bin4

</th>

<th style="text-align:right;">

age\_bin1

</th>

<th style="text-align:right;">

age\_bin2

</th>

<th style="text-align:right;">

age\_bin3

</th>

<th style="text-align:right;">

age\_bin4

</th>

<th style="text-align:right;">

diabetes\_X0

</th>

<th style="text-align:right;">

diabetes\_X1

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

<tr>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

</tr>

<tr>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0

</td>

</tr>

</tbody>

</table>

## Run tidycorels

We can now run `tidycorels::tidy_corels()` function on the prepared
diabetes training data.

``` r
diabetes_train_model <-
  tidycorels::tidy_corels(
    df = diabetes_train_preprocessed,
    outcome_cols = c("diabetes_X0", "diabetes_X1"),
    run_bfs = TRUE,
    calculate_size = TRUE,
    run_curiosity = TRUE,
    regularization = 0.01,
    curiosity_policy = 3,
    map_type = 1
  )
```

Here are the Corels rules for the diabetes data.

``` r
diabetes_train_model$corels_console_output[4:10]
```

    ## [1] "OPTIMAL RULE LIST"                             
    ## [2] "if ({age:bin1}) then ({diabetes:X0})"          
    ## [3] "else if ({glucose:bin4}) then ({diabetes:X1})" 
    ## [4] "else if ({insulin:bin1}) then ({diabetes:X0})" 
    ## [5] "else if ({pedigree:bin1}) then ({diabetes:X0})"
    ## [6] "else if ({triceps:bin4}) then ({diabetes:X1})" 
    ## [7] "else ({diabetes:X0})"

And here are those rules converted to data.table code.

``` r
diabetes_train_model$DT_code
```

    ## [1] "DT[,corels_pred := fifelse( `age_bin1` == 1, 0,fifelse( `glucose_bin4` == 1, 1,fifelse( `insulin_bin1` == 1, 0,fifelse( `pedigree_bin1` == 1, 0,fifelse( `triceps_bin4` == 1, 1,0)))))]"

A dataframe of just the true outcome, the columns used in the corels
rules, and the corels predictions is also available. The columns have
been ordered for you to work well in an
[alluvial](https://github.com/erblast/easyalluvial/blob/master/README.md)
plot.

``` r
p <- diabetes_train_model$alluvial_DT %>% 
  easyalluvial::alluvial_wide(stratum_width = 0.2) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Corels if-then-else logic",
      subtitle = " From truth (far left columun) to Corels classification (far right column)"
    )

ggplot2::ggsave(
  file = paste0(here::here(),"/images/train_alluvial.svg"),
  device = "svg",
  plot = p
)
```

<img src="./images/train_alluvial.svg" width="80%" />

## Performance on test data

Before we can apply the Corels rules to unseen test data, we apply the
same data preperation `recipe` created on the training data to the test
data.

``` r
diabetes_test_juiced <-
  diabetes_recipe %>%
  recipes::prep(diabetes_test) %>%
  recipes::juice()
```

Next we use the function `tidycorels::corels_predict()` to apply the
rules created on the training data to the test data.

``` r
diabetes_test_predict <-
  predict_corels(
    model = diabetes_train_model,
    new_df = diabetes_test_juiced
  )
```

Then plot a confusion matrix to assess the perfomrance of Corels rules
on the test data.

``` r
conf_matrix <-
  diabetes_test_predict$alluvial_DT %>%
  yardstick::conf_mat(
    truth = "diabetes_X1",
    estimate = "corels_pred"
  )

p <- ggplot2::autoplot(conf_matrix, "heatmap")
ggplot2::ggsave(
  file = paste0(here::here(),"/images/heat.svg"),
  device = "svg",
  plot = p
)
```

<img src="./images/heat.svg" width="70%" />

The confusion matrix can be used to generate the perfomance statistics
below. The accuracy achieved by Corels on the unseen test data is 0.760.
This is higher than the random forest model used in Rebbeca’s example
which reached an accuray of 0.745 on the same test data set.

``` r
summary(conf_matrix) %>%
  dplyr:::mutate(.estimate = round(.estimate,digits = 3)) %>% 
  dplyr::select(.metric,.estimate) %>% 
  dplyr::filter(.metric %in% c("accuracy","precision","recall","f_meas")) %>% 
  dplyr::mutate(.estimate = color_tile("white", "orange")(.estimate)) %>%
  kableExtra::kable(escape = F) %>%
  kableExtra::kable_styling("hover", full_width = F)
```

<table class="table table-hover" style="width: auto !important; margin-left: auto; margin-right: auto;">

<thead>

<tr>

<th style="text-align:left;">

.metric

</th>

<th style="text-align:left;">

.estimate

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

accuracy

</td>

<td style="text-align:left;">

<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffffff">0.760</span>

</td>

</tr>

<tr>

<td style="text-align:left;">

precision

</td>

<td style="text-align:left;">

<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffe3b1">0.788</span>

</td>

</tr>

<tr>

<td style="text-align:left;">

recall

</td>

<td style="text-align:left;">

<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffa500">0.852</span>

</td>

</tr>

<tr>

<td style="text-align:left;">

f\_meas

</td>

<td style="text-align:left;">

<span style="display: block; padding: 0 4px; border-radius: 4px; background-color: #ffc55b">0.819</span>

</td>

</tr>

</tbody>

</table>

## Alluvial plot

In contrast to the random forest model with variable importance values,
the Corels rules are easy to understand and visualise with the
interactive alluvial plot. Below is a screenshot where one path trough
the Corels rules is highlighted in darker green where individiuals known
to have diabetes are also correctly classified. We can see they are,

1.  in older age groups,
2.  have the highest blood glucose concentraion,
3.  have insulin that is one of the three highest bins,
4.  their pedigree (family history) is in one of the three highest value
    bins, and
5.  they are in the highest triceps bin (skin fold thickness).

<!-- end list -->

``` r
p <- easyalluvial::alluvial_wide(diabetes_test_predict$alluvial_DT)

parcats::parcats(p,
  marginal_histograms = FALSE,
  data_input = diabetes_test_predict$alluvial_DT, labelfont = 14
)
```

<img src="./images/diabetes_path.png" width="80%" />

Further, we can build a richer undestanding of each Corels rule by
visualising the distribution of values of each bin used in each rule
within the discretised/binnned column it came from. Below, each plot
from left to right highlights the bin used in each Corels rule in
sequential order.

<img src="./images/final.svg" width="80%" />

> The plot above is created in the code below by combining the
> [discretised](https://recipes.tidymodels.org/reference/step_discretize.html)
> and one-hot encoded data with the raw column values.

``` r
diabetes_recipe_non_dummy <-
  recipes::recipe(diabetes ~ pregnant + glucose + pressure + triceps +
    insulin + mass + pedigree + age,
  data = diabetes_clean
  ) %>%
  recipes::step_normalize(all_numeric()) %>%
  recipes::step_knnimpute(all_predictors()) %>%
  # discretise numeric variables into bins
  recipes::step_discretize(pregnant, glucose, pressure, triceps, insulin, mass, pedigree, age, min_unique = 1) %>%
  recipes::step_mutate_at(recipes::all_predictors(), fn = list(~ as.factor(.))) %>%
  recipes::step_mutate_at(recipes::all_outcomes(), fn = list(~ as.factor(.)))

diabetes_train_preprocessed <-
  diabetes_recipe_non_dummy %>%
  recipes::prep(diabetes_train) %>%
  recipes::juice()

combined <- diabetes_train_preprocessed %>%
  dplyr::rename_with(toupper) %>%
  dplyr::bind_cols(diabetes_train)

plot_fun <- function(X, Y, bin) {
  X <- rlang::ensym(X)
  Y <- rlang::ensym(Y)
  bin <- rlang::ensym(bin)

  combined %>%
    ggplot2::ggplot(aes(
      x = !!X,
      y = !!Y
    )) +
    ggplot2::geom_violin(scale = "count") +
    gghighlight::gghighlight(!!X == bin) +
    ggplot2::theme_minimal()
}

p1 <- plot_fun(X = AGE, Y = age, bin = bin1)
p2 <- plot_fun(X = GLUCOSE, Y = glucose, bin = bin4)
p3 <- plot_fun(X = INSULIN, Y = insulin, bin = bin1)
p4 <- plot_fun(X = PEDIGREE, Y = pedigree, bin = bin1)
p5 <- plot_fun(X = TRICEPS, Y = triceps, bin = bin4)
final <- cowplot::plot_grid(p1, p2, p3, p4, p5)

ggplot2::ggsave(
  file = "/images/final.svg",
  device = "svg",
  plot = final
)
```
