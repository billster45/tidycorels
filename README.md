tidycorels
================

  - [What are corels and tidycorels?](#what-are-corels-and-tidycorels)
  - [Installation](#installation)
  - [Simple example](#simple-example)
      - [Prepare dataframe for Corels](#prepare-dataframe-for-corels)
      - [Alluvial plot](#alluvial-plot)
      - [Performance on training data](#performance-on-training-data)
      - [Performance on new data](#performance-on-new-data)

## What are corels and tidycorels?

> Corels are [‘Certifiably Optimal RulE
> ListS’](https://corels.eecs.harvard.edu/). They are short and simple
> human [interpretable rule lists](https://arxiv.org/pdf/1704.01701.pdf)
> created on categorical data.

The Corels authors
[say](https://corels.eecs.harvard.edu/corels/whatarerulelists.html)
that.. *“The advantage of rule lists as predictive models is that they
are human interpretable, as opposed to black box models such as neural
nets. Proponents of such non-interpretable models often claim that they
are necessary to achieve greater accuracy, but we have shown that it is
possible for rule lists to have comparable or even greater accuracy.”*

`tidycorels::tidy_corels()` converts your dataframe into two text files
in the format the R package
[corels](https://cran.r-project.org/package=corels) expects. It returns
the Corels rules as well as applying them to your dataframe using
[`data.table::fifelse()`](https://rdatatable.gitlab.io/data.table/reference/fifelse.html)
code. Your dataframe is also returned with fewer columns ready to be
used in an insightful
[alluvial](https://github.com/erblast/easyalluvial/blob/master/README.md)
plot. It can reveal intuitively both how the Corels rules are applied to
your dataframe, and where the classification is correct or incorrect.
This reduced dataframe includes: the true label, the columns used in the
Corels rules in rule order, then the label column the rules create.

## Installation

``` r
devtools::install_github("billster45/tidycorels")
```

## Simple example

Let’s use tidycorels to classify cars in `datasets::mtcars` as either
automatic or manual.

### Prepare dataframe for Corels

Using [`recipes`](https://recipes.tidymodels.org/) functions in
[`tidymodels`](https://www.tidymodels.org/), columns with continuous
values are binned or
[discretised](https://recipes.tidymodels.org/reference/step_discretize.html)
into categorical data. Each bin in each column is given its own 0/1
binary column using
[\`recipes::step\_dummy()](https://recipes.tidymodels.org/reference/step_dummy.html).
This is sometimes called one-hot encoding.

Corels also requires the label column is split into two columns
representing each class. In this example, `am` is the label for Corels
to classify where 0 = automatic and 1 = manual gears.

``` r
library(tidymodels)
library(corels)
library(tidycorels)
library(easyalluvial)
library(parcats)

mtcars_recipe <-recipes::recipe(am ~ ., data = datasets::mtcars) %>%
  # 1 discretise continous variables into bins
  recipes::step_discretize(mpg, disp, hp, drat, wt, qsec, min_unique = 1) %>% 
  # 2 convert each value of each predictor into its own 0/1 binary column
  recipes::step_mutate_at(recipes::all_predictors(), fn = list(~ as.factor(.))) %>%
  recipes::step_dummy(recipes::all_predictors(), one_hot = TRUE) %>%
  # 3 convert each value of the outcome column into its own 0/1 binary column
  recipes::step_integer(recipes::all_outcomes(), zero_based = TRUE) %>% # ensure outcome is 0/1 rather than words
  recipes::step_mutate_at(recipes::all_outcomes(), fn = list(~ as.factor(.))) %>%
  recipes::step_dummy(recipes::all_outcomes(), one_hot = TRUE)

# Train the recipe
mtcars_recipe_prep <- recipes::prep(mtcars_recipe, 
                                    training = datasets::mtcars,
                                    retain = TRUE)

# Extract the pre-processed data
mtcars_preprocessed <- recipes::juice(mtcars_recipe_prep)
```

The prepared dataframe can now be used in the
`tidycorels::tidy_corels()` function. We need to specify the names of
the two columns that represent the label, and the delimiter that
seperates the column name from the value each dummy column represents.
The default seperator used by
[\`recipes::step\_dummy()](https://recipes.tidymodels.org/reference/step_dummy.html)
is an underscore.

All other arguments of `corels::corels()` are available to set, other
than the following that are fixed by `tidycorels::tidy_corels()`:
`rules_file` (generated from `df`), `labels_file` (generated from `df`),
`log_dir` (set as `getwd()`), `verbosity_policy` (set as “minor”).

``` r
corels_mtcars <-
  tidycorels::tidy_corels(
    df = mtcars_preprocessed,
    label_cols = c("am_X0", "am_X1"),
    value_delim = "_",
    run_bfs = TRUE,
    calculate_size = TRUE,
    run_curiosity = TRUE,
    regularization = 0.01,
    curiosity_policy = 3,
    map_type = 1
  )
```

A list of useful objects is returned. First let’s view the rules
captured from the console output of `corels::corels()`.

``` r
corels_mtcars$corels_console_output[4:8]
```

    ## [1] "OPTIMAL RULE LIST"                  "if ({gear:X3}) then ({am:X0})"     
    ## [3] "else if ({wt:bin1}) then ({am:X1})" "else if ({vs:X0}) then ({am:X1})"  
    ## [5] "else ({am:X0})"

And see those rules converted to `data.table::fifelse()` code.

``` r
corels_mtcars$corels_rules_DT
```

    ## [1] "DT[,corels_label := fifelse( `gear_X3` == 1, 0,fifelse( `wt_bin1` == 1, 1,fifelse( `vs_X0` == 1, 1,0)))]"

### Alluvial plot

A dataframe of just the true label, the columns used in the Corels
rules, and the Corels label from those rules is also available. The
columns have been ordered in the order of the Corels rules so that they
work well in an
[alluvial](https://github.com/erblast/easyalluvial/blob/master/README.md)
plot.

``` r
corels_mtcars$alluvial_df %>%
  easyalluvial::alluvial_wide(stratum_width = 0.2) +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Corels if-then-else logic",
    subtitle = " From truth (far left column) to Corels classification (far right column)"
  )
```

The leftmost column is the true label for each car, either automatic (0)
or manual (1). The rightmost column is the Corels classification label
after applying the rules from left to right.

The alluvial plot clearly shows which rules are important in the
classifcation and exactly how the classifiation is arrived at. For
example, if we follow one green path of manual cars (am\_X1 = 1), they
have more than 3 gears (gear\_X3 = 0), low weight (wt\_bin\_1 ==1) a V
shaped engine (vs\_x0 = 0).

We can also create an interactive version of the alluvial plot.

``` r
p <- easyalluvial::alluvial_wide(corels_mtcars$alluvial_df)

parcats::parcats(p,
  marginal_histograms = TRUE,
  data_input = corels_mtcars$alluvial_df
)
```

### Performance on training data

Either of the dataframes returned can be used to create a confusion
matrix to examine performance.

``` r
conf_matrix <-
  corels_mtcars$alluvial_df %>%
  yardstick::conf_mat(
    truth = "am_X1",
    estimate = "corels_label"
  )

ggplot2::autoplot(conf_matrix, "heatmap")
```

### Performance on new data

Use `tidycorels::predict_corels()` to apply the Corels rules to a new
dataframe (e.g. test data). It also returns the smaller data frame
intended for an
[alluvial](https://github.com/erblast/easyalluvial/blob/master/README.md)
plot. Examples can be found in the Articles section.
