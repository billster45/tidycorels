#' write_to_txt_file
#' converts columns of a dataframe to a textfile in the format corels can use
#' @param DT name of the dataframe to convert
#' @param txt_file the name of the text file
#' @return the data frame as a text file
#' @import data.table
#' @keywords internal
#' @noRd
write_to_txt_file <- function(dt,txt_file) {
  # https://github.com/Rdatatable/data.table/issues/2053 making sure @imports data.table is present
  vars <- NULL

  file.create(txt_file) # create a blank text file

  dt <- data.table::transpose(dt, keep.names = "vars") # reshape long to wide

  dt <- data.table::setDT(dt)

  dt[, vars := gsub("[_]",":",vars)] # replace under score with ':'

  dt[, vars := paste0("{", vars, "}")] # put curly brackest around the variable name

  data.table::fwrite(x = dt, file = txt_file, sep = " ", col.names = FALSE) # write to text file

}

#' capture_corels
#' converts captured corels output to dplyr::Case_when() code
#' @param corels_output corels captured output
#' @param outcome_cols the outcome columns
#' @return the corels rules as DT code
#' @keywords internal
#' @noRd
capture_corels <- function(corels_output, outcome_cols) {

  clean <- base::subset(corels_output,base::grepl(pattern = "^if*|^else*", x = corels_output))  # keep only rules

  rule_count = base::length(clean) # count rules

  clean <- base::gsub("then", "== 1,", clean) # replace "then" with comma

  clean <- base::gsub("\\{|\\}", "`", clean) # remove curly brackets and replace with back ticks

  clean <- base::gsub("\\(|\\)", "", clean) # remove brackets

  clean <- base::gsub(":", "_", clean) # replace colon with underscore

  clean <- base::gsub("^if ", "DT[,corels_pred := fifelse( ", clean) # replace the first if

  clean <- base::gsub("^else if ", "fifelse( ", clean) # replace the else if

  clean <- base::gsub(pattern = outcome_cols[1], replacement = base::substr(x = outcome_cols[1],
                                                                            start = nchar(outcome_cols[1]) ,
                                                                            stop = nchar(outcome_cols[1])),
                      x = clean) # replace outcome with outcome value

  clean <- base::gsub(pattern = outcome_cols[2], replacement = base::substr(x = outcome_cols[2],
                                                                            start = nchar(outcome_cols[2]) ,
                                                                            stop = nchar(outcome_cols[2])),
                      x = clean) # replace outcome with outcome value


  clean <- base::gsub('"','', clean) # replace quotes

  clean <- base::gsub('`1`','1', clean) # remove back ticks from the outcome value 1
  clean <- base::gsub('`0`','0', clean) # remove back ticks from the outcome value 0

  clean <- paste0(clean,",") # add commas between conditions

  clean <- paste(clean, collapse = "") # collapse into single string

  clean <- base::gsub("else ","", clean) # remove final else

  clean <- base::gsub(",$","", clean) # remove final bracket

  end_brackets <- paste(base::rep(")",rule_count-1), collapse = "") # create right number of brackets for the rules

  rules <- paste0(clean,end_brackets,"]") # build the final data.table ifelse code

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
#' @export
#' @examples
#' # See https://github.com/billster45/tidycorels/blob/master/README.md
tidy_corels <- function(df, outcome_cols, ...) {

  alluvial_DT <- NULL

  outs <- df[,outcome_cols]
  preds <- df[ , !(names(df) %in% outcome_cols)]

  out_DT <- data.table::setDT(outs)
  preds_DT <- data.table::setDT(preds)

  tempdir <- getwd()

  # write only df predictor columns to a training text file
  #pred_cols <- DT[, paste0(outcome_cols):=NULL]

  train_text_file <- paste0(tempdir, "/train.txt")

  write_to_txt_file(
    dt = preds_DT,
    txt_file = train_text_file
  )

  # write only the two label columns to a text file
  label_text_file <- paste0(tempdir, "/labels.txt")

  #out_cols <- DT[, ..outcome_cols]

  write_to_txt_file(
    dt = out_DT,
    txt_file = label_text_file
  )

  # use text files in corels to make prediction
  corels_output <-
    utils::capture.output({ # capture output to convert into code
      corels::corels(
        rules_file = train_text_file,
        labels_file = label_text_file,
        log_dir = tempdir,
        verbosity_policy = "minor",
        ...
      )
    })

  #print the normal corels output to the console
  print(corels_output)

  DT <- data.table::setDT(df)

  # convert rules to dplyr::case_when() logic
  DT_code <- capture_corels(corels_output = corels_output,
                            outcome_cols = outcome_cols)

  # apply DT code to the data frame
  base::eval(base::parse(text = DT_code)) # execute the dplyr code to create df_pred

  # alluvial data frame
  pattern <- "`\\s*(.*?)\\s*`" # regex to find word between back ticks
  corels_predictors <- regmatches(DT_code, gregexpr(pattern, DT_code)) # extract columns between back ticks
  corels_predictors <- paste(corels_predictors[[1]], collapse = ",") # collapse
  corels_predictors <- paste0("alluvial_DT <- DT[, .(",outcome_cols[2],",",corels_predictors,",corels_pred)]")

  # apply DT code to the data frame
  base::eval(base::parse(text = corels_predictors)) # execute the DT code to create alluvial_DT
  # set all columns as factors for good plotting
  for(col in colnames(alluvial_DT))
    data.table::set(alluvial_DT, j = col, value = as.factor(alluvial_DT[[col]]))

  return(c(list(
    DT_pred = data.table::setDF(DT),
    corels_console_output = corels_output,
    DT_code = DT_code,
    alluvial_DT = data.table::setDF(alluvial_DT)
)))
}

#' predict_corels
#'
#' Applies the Corels rules in theR object returned by tidycorels::corels() to a new dataframe. For example, test data not used to create the Corels rules.
#' @param model R object created by tidy_corels() function.
#' @param new_df A new dataframe to apply corels rules to and generate a classification.
#' @export
#' @examples
#' # See https://github.com/billster45/tidycorels/blob/master/README.md
predict_corels <- function(model, new_df) {

  alluvial_DT <- NULL

  df_name <- base::deparse(base::substitute(new_df)) #https://stackoverflow.com/a/45176503

  new_df <- data.table::setDT(new_df)

  code <- base::gsub(pattern = "DT\\[", replacement = base::paste0(df_name,"\\["), model$DT_code)

  base::eval(base::parse(text = code))

  cols <- colnames(model$alluvial_DT)

  # alluvial data frame
  corels_predictors <- paste(cols, collapse = ",") # collapse
  corels_predictors <- paste0("alluvial_DT <-",df_name,"[, .(",corels_predictors,")]")

  # apply DT code to the data frame
  base::eval(base::parse(text = corels_predictors)) # execute the DT code to create alluvial_DT
  # set all columns as factors for good plotting
   for(col in colnames(alluvial_DT))
     data.table::set(alluvial_DT, j = col, value = as.factor(alluvial_DT[[col]]))

  return(c(list(
    new_df = data.table::setDF(new_df),
   alluvial_DT = data.table::setDF(alluvial_DT)
  )))
}
