#' write_to_txt_file
#' converts columns of a dataframe to a textfile in the format corels can use
#' @param DT name of the dataframe to convert
#' @param txt_file the name of the text file
#' @param value_delim the delimiter to mark where the value of the 0/1 binary feature appears in the column name
#' @return the data frame as a text file
#' @import data.table
#' @keywords internal
#' @noRd
write_to_txt_file <- function(df, txt_file, value_delim) {
  # https://github.com/Rdatatable/data.table/issues/2053 making sure @imports data.table is present
  vars <- NULL

  file.create(txt_file) # create a blank text file

  DT <- data.table::setDT(df)

  # swap rows with columns as Corels expects
  DT <- data.table::transpose(DT, keep.names = "vars")

  # replace the value delimiter in the column name with a colon as Corels expects
  DT[, vars := base::sub(
    pattern = base::paste0("[", value_delim, "]"),
    replacement = ":",
    x = vars
  )]

  DT[, vars := base::paste0("{", vars, "}")] # put curly brackest around the variable name as Corels expects

  # write to text file ready for input into corels::corels() function
  data.table::fwrite(
    x = DT,
    file = txt_file,
    sep = " ",
    col.names = FALSE
  )
}

#' capture_corels
#' converts captured Corels rules outputted to console to data.table::fifelse() code
#' @param corels_output corels captured output
#' @param label_cols the outcome columns
#' @return the corels rules as DT code
#' @keywords internal
#' @noRd
capture_corels <- function(corels_output, label_cols, value_delim) {
  clean <- base::subset(corels_output, base::grepl(pattern = "^if*|^else*", x = corels_output)) # keep only rules

  rule_count <- base::length(clean) # count the number of Corels rules so we know how many brackets to put at end of ifelse logic

  clean <- base::sub("then", "== 1,", clean) # replace "then" with comma

  clean <- base::gsub("\\{|\\}", "`", clean) # remove curly brackets around column names and replace with back ticks

  clean <- base::gsub("\\(|\\)", "", clean) # remove brackets

  clean <- base::gsub(":", value_delim, clean) # replace colon with the delimiter provided by user of the function

  clean <- base::sub("^if ", "DT[,corels_label := fifelse( ", clean) # replace the first if with DT ifelse

  clean <- base::sub("^else if ", "fifelse( ", clean) # replace the else if with DT ifelse after a comma

  clean <- base::sub(
    pattern = label_cols[1], replacement = base::substr(
      x = label_cols[1],
      start = nchar(label_cols[1]),
      stop = nchar(label_cols[1])
    ),
    x = clean
  ) # replace outcome with outcome value

  clean <- base::sub(
    pattern = label_cols[2], replacement = base::substr(
      x = label_cols[2],
      start = nchar(label_cols[2]),
      stop = nchar(label_cols[2])
    ),
    x = clean
  ) # replace outcome with outcome value


  clean <- base::gsub('"', "", clean) # remove quotes

  clean <- base::gsub("`1`", "1", clean) # remove back ticks from the outcome value 1
  clean <- base::gsub("`0`", "0", clean) # remove back ticks from the outcome value 0

  clean <- paste0(clean, ",") # add commas between conditions

  clean <- paste(clean, collapse = "") # collapse into single string

  clean <- base::gsub("else ", "", clean) # remove final else

  clean <- base::gsub(",$", "", clean) # remove final bracket

  end_brackets <- paste(base::rep(")", rule_count - 1), collapse = "") # create right number of brackets for the rules

  rules <- paste0(clean, end_brackets, "]") # build the final data.table ifelse code

  return(rules)
}

#' diagram_corels
#' creates four data frames: one for the alluvial plot, one for rule performance, and two for a sankey diagram
#' @param DT the data table of only the colums to visualise
#' @return the data table with information not used in the rules removed and nodes and edges for a sankey diagram
#' @keywords internal
#' @noRd
diagram_corels <- function(DT) {
  # store the column names
  cols <- colnames(DT)
  colcount <- length(cols) - 1
  keep_cols <- cols[2:colcount]
  rule_count <- length(keep_cols)
  first_col <- cols[1]
  last_col <- cols[length(cols)]

  ## 1. Remove information not used (i.e. a rule has fired and later rule values not used)
  DT1 <- DT[, keep_cols, with = FALSE] # keep only columns used in Corels rules
  DT1[, ID := .I] # create ID per row
  DT1 <- data.table::melt(DT1, id.vars = "ID", variable.name = "rule", value.name = "value") # reshape wide to long
  DT1 <- DT1[order(ID)] # order by the ID
  DT1[, index := 1:.N, by = ID] # add an index column per event for each ID
  # If the vale of a rule is 1, then make the value of the next rule NA as it's not used in the Corels rules
  DT1[, value := ifelse(index > 1 & data.table::shift(value,n =1, type = "lag") ==1, yes = NA, no = value), by = ID]
  # If the vale of a rule is NA, then make the value of the next rule NA as it's not used in the Corels rules
  for (col in 1:length(cols)){
    DT1[, value := ifelse(index > 1 & is.na(data.table::shift(value,n =1, type = "lag")), yes = NA, no = value), by = ID]
  }
  DT1_wide <- data.table::dcast(DT1, ID ~ rule, value.var = "value") # rehsape from long to wide

  ## 2. create a dataframe to be returned for an alluvial plot
  alluvial_DT <- cbind(DT[,..first_col], DT1_wide, DT[,..last_col]) # combine the NA'd rule values back alongside the first and last columns (truth and corels label respectively)
  alluvial_DT <- alluvial_DT[,!c("ID")] # drop id no longer needed

  ## 3. Create a dataframe that shows classification success by rule fired
  DT_t <- DT1[, rule := ifelse(value == 1, yes = as.character(rule), no = NA)] # only keep rules if fired
  DT_t[, rule := ifelse(value == 0 & index == rule_count, yes = "else", no = rule)] # the final else rule
  DT_t <- DT_t[!is.na(rule),]  # remove rules if not fired
  DT_t <- DT_t[,c("rule")] # keep just the column of rules fired
  success_DT <- cbind(DT[,..first_col], DT_t, DT[,..last_col]) # combine the rules fired with the truth outcome and corels_label
  code <- paste0("success_DT <- success_DT[,correct := ifelse(",first_col," == corels_label, 1, 0)]") # create correct = 1 if rule is right
  base::eval(base::parse(text = code)) # execute the code above
  rule_performance <- success_DT[, .(rule_fire_count = .N, rule_correct = sum(correct)), by = rule] # calculate how many times each rule was right
  rule_performance[,rule_perc_correct := (rule_correct/rule_fire_count) * 100] # calculate % right

  DT_rule_order <- data.table::data.table(rule=c(keep_cols,"else")) # create table of the rules in the order they appear
  DT_rule_order[, ID := .I] # as an ID

  rule_performance <- rule_performance[DT_rule_order, on =.(rule = rule)] # join rule order table to sort performance table
  rule_performance <- rule_performance[,!c("ID")] # drop id no longer needed

  ## 4. SANKEY diagram table preperation
  DT2 <- cbind(DT1_wide, DT[,..last_col]) # combine the NA'd rule values alongside the corels label
  DT2 <- data.table::melt(DT2, id.vars = "ID",variable.name = "rule", value.name = "value") # reshape wide to long
  DT2 <- DT2[order(ID)]
  DT2 <- stats::na.omit(DT2) # remove rules not fired
  DT2[, source := paste0(rule," = ",value)] # combine the rule and the value
  DT2 <- DT2[, c("ID", "source")] # keep only these two columns

  ## 5. Sankey NODES
  nodes <- DT2[, keyby = .(source), .(node_n = .N)] # summarise the rules as nodes
  nodes <- nodes[, !c("node_n")] # remove the node count
  nodes[, ID := .I -1] # set the node ID to start at 0 as networkD3::sankeyNetwork() require this
  data.table::setnames(nodes, old = "source", new = "label")

  ## 6. Sankey EDGES
  DT2[, target := data.table::shift(source, -1), by = ID] # create a new column that is the next rule
  DT2 <- stats::na.omit(DT2) # get rid of final row that is NA in the column target
  edges <- DT2[, keyby = .(source, target), .(value = .N)] # create the count between nodes
  edges <- nodes[edges, on=c(label = "source")] # join the nodes table to replace source with its node ID
  data.table::setnames(edges, old = "ID", new = "source") # rename the source node ID to source
  edges <- nodes[edges, on=c(label = "target")] # join the nodes table to replace target with its node ID
  data.table::setnames(edges, old = "ID", new = "target") # rename the target node ID to target
  edges <- edges[, .(source,target,value)] # keep only relevant columns

  return(c(list(
    alluvial_DT = alluvial_DT,
    rule_performance = rule_performance,
    nodes = nodes,
    edges = edges
  )))
}
#' tidy_corels
#'
#' The tidy_corels() function converts your dataframe into the text file format corels::corels() expects and returns a list of useful R objects.
#'
#' The function returns your dataframe with the corels rules applied by adding the column "corels_label".
#'
#' Returns the Corels rules from the console output of corels::corels().
#'
#' Returns those rules converted to data.table::fifelse() code (corels_rules_DT).
#'
#' Returns a dataframe of only the true label, the columns with their value if they are used by the rules (i.e. if the value is not used it will be NA), and the corels classification applied to your dataframe (alluvial_df). This dataframe is intended to be used in an easyalluvial plot \url{https://github.com/erblast/easyalluvial/blob/master/README.md}.
#'
#' Returns a dataframe of the peformance of each rule in the order of Corels rules (rule_performance_df).
#'
#' Returns two dataframes (sankey_edges_df and sankey_nodes_df) to create a sankey network diagram of the corels rules \url{https://christophergandrud.github.io/networkD3/#sankey}.
#'
#' All variables must contain 0 or 1. Consider using recipes::step_dummy() to convert categorical variables to dummy columns. If variable is continous (e.g. age), consider using recipes::step_discretize() before recipes::step_dummy().
#'
#' The names of the two outcome columns in label_cols argument must end in 0 and 1.
#'
#' Consider using recipes::step_integer(recipes::all_outcomes(), zero_based = TRUE) to convert outcome values to 0/1 before using recipes::step_dummy() to create the two outcome columns Corels requires.
#'
#' To apply the corels rules created to a new dataframe, see \code{\link[tidycorels]{predict_corels}}.
#'
#' \strong{Examples}: \url{https://billster45.github.io/tidycorels/}
#' @param df The dataframe to generate rules from. Applies the function corels::corels().
#' @param label_cols The two columns in df that represent the label. Corels expects two columns representing each class. Consider using recipes::step_dummy() to convert outcome to two columns.
#' @param value_delim the delimiter to mark where the value of the 0/1 binary feature appears in the column name. Default value is underscore "_" as this is used by recipes::step_dummy() when creating dummy columns.
#' @param ... Set any of the arguments imported from corels::corels(). The following arguments are fixed by tidy_corels(): rules_file and labels_file (both generated from the dataframe supplied to the df argument), log_dir (set as your current working directory), verbosity_policy (set as "minor").
#' @export
tidy_corels <- function(df, label_cols, value_delim = "_", ...) {
  alluvial_DT <- NULL

  tempdir <- getwd()

  # write only df predictor columns to a training text file
  train_text_file <- paste0(tempdir, "/train.txt")

  write_to_txt_file(
    df = base::subset(df, select = !(names(df) %in% label_cols)),
    txt_file = train_text_file,
    value_delim = value_delim
  )

  # write only the two label columns to a text file
  label_text_file <- paste0(tempdir, "/labels.txt")

  write_to_txt_file(
    df = base::subset(df, select = label_cols),
    txt_file = label_text_file,
    value_delim = value_delim
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

  # print the normal corels output to the console
  print(corels_output)

  DT <- data.table::setDT(df)

  # convert rules to data.table ifelse logic
  DT_code <- capture_corels(
    corels_output = corels_output,
    label_cols = label_cols,
    value_delim = value_delim
  )

  # apply DT code to the data frame
  base::eval(base::parse(text = DT_code)) # execute the dplyr code to create df_pred

  # alluvial data frame
  single_label <- label_cols[base::grepl("1", label_cols)] # keep only the label column name that has a number 1 in
  pattern <- "`\\s*(.*?)\\s*`" # regex to find word between back ticks
  corels_predictors <- base::regmatches(DT_code, base::gregexpr(pattern, DT_code)) # extract columns between back ticks
  corels_predictors <- paste(corels_predictors[[1]], collapse = ",") # collapse
  corels_predictors <- paste0("alluvial_DT <- DT[, .(", single_label, ",", corels_predictors, ",corels_label )]")

  # apply DT code to the data frame
  base::eval(base::parse(text = corels_predictors)) # execute the DT code to create alluvial_DT

  # remove information from the alluvial data frame not used by the corels rules. And create nodes
  # and edges for sankey diagram
  diagrams <- diagram_corels(DT = alluvial_DT)

  # set all columns as factors for plotting
  for (col in colnames(diagrams$alluvial_DT)) {
    data.table::set(diagrams$alluvial_DT, j = col, value = as.factor(diagrams$alluvial_DT[[col]]))
  }

  for (col in colnames(DT)) {
    data.table::set(DT, j = col, value = as.factor(DT[[col]]))
  }

  return(c(list(
    df_labelled = data.table::setDF(DT),
    corels_console_output = corels_output,
    corels_rules_DT = DT_code,
    alluvial_df = data.table::setDF(diagrams$alluvial_DT),
    rule_performance_df = data.table::setDF(diagrams$rule_performance),
    sankey_nodes_df = data.table::setDF(diagrams$nodes),
    sankey_edges_df = data.table::setDF(diagrams$edges)
  )))
}

#' predict_corels
#'
#' Applies the Corels rules returned by \code{\link[tidycorels]{tidy_corels}} to a new dataframe. For example, test data not used to create the Corels rules.
#'
#' The function returns your dataframe with the corels rules applied by adding the column "corels_label".
#'
#' Returns a dataframe of only the true label, the columns used in the rules, and the corels classification applied to the new dataframe (alluvial). This dataframe is intended to be used in an easyalluvial plot \url{https://github.com/erblast/easyalluvial/blob/master/README.md}.
#'
#' \strong{Examples}: \url{https://billster45.github.io/tidycorels/}
#'
#' @param model R object created by tidy_corels() function.
#' @param new_df A new dataframe to apply corels rules to and generate a classification.
#' @export

predict_corels <- function(model, new_df) {
  alluvial_DT <- NULL

  new_df_DT <- data.table::setDT(new_df) # convert dataframe supplied to data.table

  # edit the corels rules in DT code so it can be run on the new datafame supplied (new_df)
  code <- base::gsub(pattern = "DT\\[", replacement = base::paste0("new_df_DT["), model$corels_rules_DT)
  base::eval(base::parse(text = code))

  # alluvial data frame
  cols <- colnames(model$alluvial)
  corels_predictors <- paste(cols, collapse = ",") # collapse
  corels_code <- paste0("alluvial_DT <- new_df_DT[, .(", corels_predictors[[1]], ")]")
  base::eval(base::parse(text = corels_code)) # execute the DT code to create alluvial_DT

  # remove information from the alluvial data frame not used by the corels rules. And create nodes
  # and edges for sankey diagram
  diagrams <- diagram_corels(DT = alluvial_DT)

  # set all columns as factors for plotting
  for (col in colnames(diagrams$alluvial_DT)) {
    data.table::set(diagrams$alluvial_DT, j = col, value = as.factor(diagrams$alluvial_DT[[col]]))
  }

  for (col in colnames(new_df_DT)) {
    data.table::set(new_df_DT, j = col, value = as.factor(new_df_DT[[col]]))
  }

  return(c(list(
    new_df_labelled = data.table::setDF(new_df_DT),
    corels_rules_DT = corels_code,
    alluvial_df = data.table::setDF(diagrams$alluvial_DT),
    rule_performance_df = data.table::setDF(diagrams$rule_performance),
    sankey_nodes_df = data.table::setDF(diagrams$nodes),
    sankey_edges_df = data.table::setDF(diagrams$edges)
  )))
}
