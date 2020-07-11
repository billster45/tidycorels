#' write_to_txt_file
#' converts columns of a dataframe to a textfile in the format corels can use
#' @param df name of the dataframe to convert
#' @param cols a list of the columns in the data frame to convert
#' @param txt_file the name of the text file
#' @return the data frame as a text file
#' @importFrom magrittr %>%
#' @keywords internal
#' @noRd
write_to_txt_file <- function(df, cols, txt_file) {
  file.create(txt_file) # create a blank text file

  for (col in cols) {
    single_col <- df %>%
      dplyr::select(tidyselect::all_of(col)) # select one column

    single_col_char <- base::as.character(single_col) # convert column to a single character string

    single_col_char <- stringr::str_replace_all(single_col_char, "[\r\n,c()]", "") # replace any text other than the values

    name <- colnames(single_col) %>% # use the column name to label the character string in the format Corels epxects
      stringi::stri_replace_last_regex(pattern = "[_]", replacement = ":", opts_regex = NULL)

    final <- paste0("{", name, "}", " ", single_col_char) # append the column name to the column values

    write(final, file = txt_file, append = TRUE) # finally append to the text file
  }
}

#' capture_corels
#' converts captured corels output to dplyr::Case_when() code
#' @param corels_output corels captured output
#' @importFrom magrittr %>%
#' @return the corels rules as dplyr code
#' @keywords internal
#' @noRd
capture_corels <- function(corels_output) {

  corels_prediction <- NULL

  lines <- length(corels_output) # find out many lines in rules

  rules <- as.character()
  for (line in 1:lines) {
    string <- corels_output[line]

    # convert the if or else if parts to dplyr
    if (stringr::str_detect(string, pattern = "if \\(|else if")) {
      string <- string %>%
        stringr::str_replace(pattern = ",", replacement = "}) & ({")

      # extract if and then parts
      then_loc <- stringr::str_locate(string, pattern = "then")
      if_condition <- stringr::str_sub(string, start = 1, end = then_loc[1] - 1)
      then_condition <- stringr::str_sub(string, start = then_loc[1], end = stringr::str_length(string))

      if_condition <-
        if_condition %>%
        stringr::str_replace_all(pattern = "\\(\\{", replacement = "`") %>%
        stringr::str_replace_all(pattern = "\\}\\)", replacement = "` == 1") %>%
        stringr::str_replace_all(pattern = "if|else if", replacement = "")

      then_colon_loc <- stringr::str_locate(then_condition, pattern = ":")

      then_condition <-
        then_condition %>%
        stringr::str_sub(start = then_colon_loc[1], end = stringr::str_length(then_condition)) %>%
        stringr::str_replace_all(pattern = "[:})]", replacement = "") %>%
        stringr::str_extract("(\\d)+") # keep only the number

      combined_if_then <- paste0(if_condition, "~ '", then_condition, "'")
      rules <- paste(rules, combined_if_then, ",")
    } else if (stringr::str_detect(string, pattern = "else \\(")) { # convert the final else to dplyr
      else_colon_loc <- stringr::str_locate(string, pattern = ":")

      else_condition <-
        string %>%
        stringr::str_sub(start = else_colon_loc[1], end = stringr::str_length(string)) %>%
        stringr::str_replace_all(pattern = "[:})]", replacement = "") %>%
        stringr::str_extract("(\\d)+") # keep only the number

      rules <- paste0(rules, " TRUE ~ '", else_condition, "'))")
    }
  }

  rules <- rules %>%
    stringr::str_replace_all(pattern = ":", replacement = "_")

  rules <- paste0("dplyr::mutate(corels_prediction = dplyr::case_when(", rules)

  return(rules)
}

#' tidy_corels
#'
#' The tidy_corels() function converts your dataframe into the text file format corels::corels() expects. Returns a list of R objects including: both the Corels rules and converted to dplyr::case_when() format, your datafame with the label geneated by the rules, an alluvial plot of the rules applied to your dataframe.
#'
#' All variables must contain 0 or 1. Consider using recipes::step_dummy() to convert categorical variables to dummy columns. If variable is continous (e.g. age), consider using recipes::step_discretize() before recipes::step_dummy().
#'
#' The names of the two outcome columns in outcome_cols argument must end in 0 and 1.
#' Consider using recipes::step_integer(recipes::all_outcomes(), zero_based = TRUE) to convert outcome values to 0/1 before using recipes::step_dummy() to create the two outcome columns Corels requires.
#'
#' @param df The dataframe to generate rules from. Applies the function corels::corels().
#' @param outcome_cols The two columns in df that represent the label. Corels expects two columns representing each class. Consider using recipes::step_dummy() to convert outcome to two columns.
#' @param ... Set any of the arguments imported from corels::corels(). The following arguments are fixed by tidy_corels(): rules_file (generated from df), labels_file (generated from df), log_dir (set as tempdir()), verbosity_policy (set as "minor").
#' @importFrom magrittr %>%
#' @export
#' @examples
#'
#' library(magrittr)
#' # Using mtcars dataset and recipes, create binary predictors as Corels expects
#'
#' corels_pre_proc <-
#'   recipes::recipe(am ~ .,
#'                   data = datasets::mtcars
#'   ) %>%
#'   # discretise numeric variables into bins
#'   recipes::step_discretize(mpg, disp, hp, drat, wt, qsec, min_unique = 1) %>%
#'   recipes::step_mutate_at(recipes::all_predictors(), fn = list(~ as.factor(.))) %>%
#'   # convert each value of each category into its own 0/1 binary column
#'   recipes::step_dummy(recipes::all_predictors(), one_hot = TRUE) %>%
#'   recipes::step_nzv(recipes::all_predictors()) %>%
#'   # convert each value of the outcome into its own 0/1 binary column
#'   # ensure outcome is 0/1 rather than words
#'   recipes::step_integer(recipes::all_outcomes(), zero_based = TRUE) %>%
#'   recipes::step_mutate_at(recipes::all_outcomes(), fn = list(~ as.factor(.))) %>%
#'   recipes::step_dummy(recipes::all_outcomes(), one_hot = TRUE)
#'
#' #Create a pre-processed dataframe from the mtcars recipe above
#'
#' corels_juiced <-
#'   corels_pre_proc %>%
#'   recipes::prep() %>%
#'   recipes::juice()
#'
#' # Run the tidy_corels() function
#'
#'   corels_juiced_tidy <-
#'    tidycorels::tidy_corels(
#'      df = corels_juiced,
#'      outcome_cols = c("am_X0", "am_X1"),
#'      run_bfs = TRUE,
#'      calculate_size = TRUE,
#'      run_curiosity = TRUE,
#'      regularization = 0.01,
#'      curiosity_policy = 3,
#'      map_type = 1
#'   )
#'
#' # View the alluvial plot of the Corels rules applied to df
#'
#' corels_juiced_tidy$alluvial_plot
tidy_corels <- function(df, outcome_cols, ...) {

  df_pred <- NULL
  corels_prediction <- NULL


  tempdir <- tempdir()

  # extract only the names of df predictor columns
  pred_cols <- colnames(df %>%
    dplyr::select(-outcome_cols))

  # write only df predictor columns to a training text file
  train_text_file <- paste0(tempdir, "/train.txt")

  write_to_txt_file(
    df = df,
    cols = pred_cols,
    txt_file = train_text_file
  )

  # extract only the names of two label columns of df
  label_cols <- colnames(df %>%
    dplyr::select(outcome_cols))

  # write only the two mtcar label columns to a text file
  label_text_file <- paste0(tempdir, "/labels.txt")
  write_to_txt_file(
    df = df,
    cols = label_cols,
    txt_file = label_text_file
  )

  # use text files in corels to make prediction
  logdir <- tempdir()

  corels_console_output <-
    utils::capture.output({ # capture output to convert into code
      corels::corels(
        rules_file = train_text_file,
        labels_file = label_text_file,
        log_dir = logdir,
        verbosity_policy = "minor",
        ...
      )
    })

  # convert rules to dplyr::case_when() logic
  dplyr_code <- capture_corels(corels_output = corels_console_output)

  # apply the dplyr code to the data frame
  dplyr_code <- paste0("df_pred <- df %>% ", dplyr_code)

  base::eval(base::parse(text = dplyr_code)) # execute the dplyr code to create df_pred

  # alluvial plot
  corels_predictors <-
    dplyr_code %>%
    stringr::str_extract_all("`\\s*(.*?)\\s*`") # extracts just the column names between the back-ticks ``

  corels_predictors <-
    stringi::stri_paste_list(corels_predictors, collapse = "", sep = ",")

  alluvial_code <- paste0(
    "alluvial_df <- df_pred %>% dplyr::select(",
    label_cols[2],
    ",",
    corels_predictors,
    ",corels_prediction)"
  )

  base::eval(base::parse(text = alluvial_code))

  alluvial_df <-
    alluvial_df %>%
    dplyr::mutate(corels_prediction = as.numeric(stringr::str_extract(corels_prediction, "(\\d)+"))) %>%
    dplyr::mutate_all(as.factor)

  alluvial_plot <-
    alluvial_df %>%
    easyalluvial::alluvial_wide(stratum_width = 0.2) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Corels if-then-else logic",
      subtitle = " From truth (far left columun) to Corels classification (far right column)"
    )

  # read in text files from temp directory to output too
  train_txt <- utils::read.table(train_text_file)
  label_txt <- utils::read.table(label_text_file)

  # clean the dplyr code to just be the case_when logic before returning
  magritter_loc <- stringr::str_locate(dplyr_code, pattern = "%>%")
  dplyr_code <- stringr::str_sub(dplyr_code, start = magritter_loc[2] + 2, end = stringr::str_length(dplyr_code))

  return(c(list(
    df_pred = df_pred,
    corels_console_output = corels_console_output,
    dplyr_code = dplyr_code,
    label_cols = label_cols,
    corels_predictors = corels_predictors,
    alluvial_plot = alluvial_plot,
    alluvial_code = alluvial_code,
    alluvial_df = alluvial_df,
    train_text_file = train_txt,
    label_text_file = label_txt
  )))
}

#' predict_corels
#'
#' Applies the Corels rules in theR object returned by tidycorels::corels() to a new dataframe. For example, test data not used to create the Corels rules.
#' @param model R object created by tidy_corels() function.
#' @param new_df A new dataframe to apply corels rules to and generate a classification.
#' @export
#' @examples
#' # See example in README.md (link here)
predict_corels <- function(model, new_df) {
  df_pred <- NULL

  new_data <- base::deparse(base::substitute(new_df))

  code <- paste0("df_pred <- ", new_data, " %>% ", model$dplyr_code)

  base::eval(base::parse(text = code))

  # alluvial plot
  base::eval(base::parse(text = model$alluvial_code))

  alluvial_df <-
    alluvial_df %>%
    dplyr::mutate_all(as.factor)

  alluvial_plot <-
    alluvial_df %>%
    easyalluvial::alluvial_wide(stratum_width = 0.2) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = "Corels if-then-else logic",
      subtitle = "From truth (far left columun) to Corels classification (far right column)"
    )

  return(c(list(
    df_pred = df_pred,
    alluvial_plot = alluvial_plot,
    alluvial_df = alluvial_df
  )))
}
