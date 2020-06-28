#' write_to_txt_file
#'
#' converts columns of a dataframe to a textfile in the format corels can use
#' @param df name of the dataframe to convert
#' @param cols a list of the columns in the data frame to convert
#' @param txt_file the name of the text file
#' @keywords dataframe convert textfile
#' @export
#' @examples
#' # not run:
#' # write_to_txt_file(df = df, cols = cols, txt_file = "train.txt")
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

#' rules_to_case_when
#'
#' converts captured corels output to dplyr::Case_when()
#' @param corels_output corels captured output
#' @keywords corels capture
#' @export
#' @examples
#' # not run:
rules_to_case_when <- function(corels_output) {
  lines <- length(corels_output) # find out many lines in rules

  rules <- as.character()
  for (line in 1:lines) {
    string <- corels_output[line]

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
        stringr::str_replace_all(pattern = "[:})]", replacement = "")

      combined_if_then <- paste0(if_condition, "~ ", then_condition, "")
      rules <- paste(rules, combined_if_then, ",")
    } else if (stringr::str_detect(string, pattern = "else \\(")) {
      else_colon_loc <- stringr::str_locate(string, pattern = ":")

      else_condition <-
        string %>%
        stringr::str_sub(start = else_colon_loc[1], end = stringr::str_length(string)) %>%
        stringr::str_replace_all(pattern = "[:})]", replacement = "")

      rules <- paste0(rules, " TRUE ~ ", else_condition, "))")
    }
  }

  rules <- rules %>%
    stringr::str_replace_all(pattern = ":", replacement = "_")

  rules <- paste0("dplyr::mutate(corel_prediction = dplyr::case_when(", rules)

  return(rules)
}
