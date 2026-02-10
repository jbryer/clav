# Shiny module for cluster analysis.

Shiny module for cluster analysis.

## Usage

``` r
cluster_module(
  id,
  data,
  default_vars = names(data())[sapply(data(), function(x) {
     is.numeric(x)
 })],
  default_dependent_variable = NULL,
  se_factor = 1
)
```

## Arguments

- id:

  An ID string that corresponds with the ID used to call the module's UI
  function.

- data:

  a function to return the data (probably a reactive function).

- default_vars:

  character list for the variables to include by default.

- default_dependent_variable:

  the name of the dependent variable, or NULL for none.

- se_factor:

  how many standard errors to plot.
