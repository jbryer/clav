# Output for printing status messages from the Shiny module.

Table output for analyzing the dependent variable from the clusters.
This will either be an ANOVA for a quantitative dependent variable or a
chi-squared test for qualitative dependent variable.

## Usage

``` r
n_cluster_message(id)

cluster_method_input(id)

n_clusters_input(
  id,
  label = "Number of clusters:",
  min = 2,
  max = 10,
  value = 4
)

cluster_variable_input(id)

n_cluster_plot_output(id)

cluster_size_bar_plot_output(id, ...)

profile_plot_output(id, ...)

cluster_pairs_plot_output(id, ...)

bivariate_cluster_plot_output(id, ...)

discriminant_projection_plot_output(id, ...)

dependent_variable_input(id)

dependent_variable_plot_output(id, ...)

dependent_variable_table_output(id)

dependent_null_hypothesis_output(id)

optimal_clusters_plot_output(id)

cluster_valdiation_plot_output(id)

cluster_validation_distribution_plot_output(id)

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

- label:

  label for the slider input.

- min:

  The minimum value (inclusive) that can be selected.

- max:

  The maximum value (inclusive) that can be selected.

- value:

  The initial value of the slider.

- ...:

  other parmaeters passed to
  [`shiny::plotOutput()`](https://rdrr.io/pkg/shiny/man/plotOutput.html).

- data:

  a function to return the data (probably a reactive function).

- default_vars:

  character list for the variables to include by default.

- default_dependent_variable:

  the name of the dependent variable, or NULL for none.

- se_factor:

  how many standard errors to plot.

## See also

[`profile_plot()`](profile_plot.md)

[`GGally::ggpairs()`](https://ggobi.github.io/ggally/reference/ggpairs.html)
