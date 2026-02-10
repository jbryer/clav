# Cluster profile validation

This function takes multiple random samples from the provided data
frame, estimate cluster membership, and calculate the mean (the default,
a different statistic can be specified using the `summary_fun`
parameter, e.g. `median` may be appropriate) for each cluster using both
the in sample and out-of-bag (i.e. out of sample). For the out-of-bag
sample

This function will re-assign cluster labels such that the mean absolute
difference to the the cluster labels from the full dataset is as small
as possible.

## Usage

``` r
cluster_validation(
  df,
  n_clusters = 2,
  cluster_fun = stats::kmeans,
  get_cluster_fun = function(x) {
x$cluster
 },
  oob_predict_fun = function(fit, newdata) {
     predict(fit, newdata = newdata)
 },
  summary_fun = mean,
  n_samples = 100,
  sample_size = 0.5 * nrow(df),
  replace = FALSE,
  standardize = TRUE,
  seed,
  verbose = interactive(),
  ...
)

# S3 method for class 'clustervalidation'
plot(
  x,
  plot_complete = TRUE,
  plot_in_sample = TRUE,
  plot_oob_sample = TRUE,
  point_alpha = 0.1,
  point_size = 1,
  line_alpha = 0.1,
  line_width = 1,
  complete_color = "blue",
  complete_size = 1,
  complete_point_size = 2,
  xlab = "",
  ylab = ifelse(attr(x, "standardize"), "Mean Standard Score", "Mean Score"),
  ...
)

# S3 method for class 'clustervalidation'
print(x, ...)

# S3 method for class 'clustervalidation'
summary(object, in_sample = FALSE, oob_sample = TRUE, ...)

# S3 method for class 'cv_summary'
plot(x, ...)

fix_cluster_labels(
  cv,
  greedy = (length(unique(cv$oob_sample$cluster)) > 6),
  var,
  ...
)

plot_distributions(
  cv,
  plot_in_sample = TRUE,
  plot_oob_sample = FALSE,
  nrow = NULL,
  palette = 2,
  ...
)
```

## Arguments

- df:

  data frame to estimate clusters. Note that all columns will be used in
  the estimation.

- n_clusters:

  the number of clusters to estimate.

- cluster_fun:

  the function used to estimate the clusters.

- get_cluster_fun:

  the function used to get the cluster classes. This function takes one
  parameter, the result of `cluster_fun`.

- oob_predict_fun:

  the function used to get predictions from the out-of-bag sample.
  Function takes two parameters, the first is the results of
  `cluster_fun`, the second is the out-of-bag sample data frame.

- summary_fun:

  the function used to calculate the statistic for each cluster and
  iteration. Defaults to `mean`.

- n_samples:

  the number of random samples to draw.

- sample_size:

  the size of each random sample. Defaults to 50% of observations.

- replace:

  whether sampling should be done with replacement.

- standardize:

  whether the variables should be standardized before estimating
  clusters.

- seed:

  random number seed. Note that the seed is set before each iteration to
  `seed + i` where `i` is the iteration number.

- verbose:

  whether the function should print the status while running.

- ...:

  currently not used.

- x:

  result of `summary.clustervalidation()`

- plot_complete:

  whether the profile line using the complete data set should be
  plotted.

- plot_in_sample:

  whether to plot the in sample (i.e. bootstrap) distributions.

- plot_oob_sample:

  whether to plot the out-of-bag samples.

- point_alpha:

  the alpha (transparency) level for points.

- point_size:

  the size of the points.

- line_alpha:

  the alpha (transparency) level for lines.

- line_width:

  width of the lines.

- complete_color:

  the color of the path for the path using the complete dataset (i.e.
  `plot_complete = TRUE`)

- complete_size:

  the size of the path for the path using the complete dataset (i.e.
  `plot_complete = TRUE`)

- complete_point_size:

  the point size of the path for the path using the complete dataset
  (i.e. `plot_complete = TRUE`)

- xlab:

  label for the x-axis.

- ylab:

  label for the y-axis.

- object:

  the results of `cluster_validation`.

- in_sample:

  plot the in sample results.

- oob_sample:

  plot the out-of-bag sample results.

- cv:

  the results from `cluster_validation()`.

- greedy:

  if `FALSE` this will find the optimal cluster pattern by calculating
  difference for all combinations. If `TRUE` this will consider only one
  variable (the one with the largest variance across means).

- var:

  the variable to use if `greedy = TRUE`.

- nrow:

  number of rows. This is ignored if `plot_in_sample = TRUE` and
  `plot_oob_sample = TRUE`.

- palette:

  If a string, will use that named palette. If a number, will index into
  the list of palettes of appropriate type. See
  [`ggplot2::scale_color_brewer()`](https://ggplot2.tidyverse.org/reference/scale_brewer.html)
  for more information.

## Value

a list with the following elements:

- data:

  the original data frame.

- complete_sample:

  data frame of results using the entire data set.

- in_sample:

  data frame of in sample results.

- oob_sample:

  data frame of out-of-bag results.

- complete_model_fit:

  model fit for the full data set.

- in_sample_model_fits:

  model fits for each sample.

Each of these data frames contain four columns:

- iter:

  the iteration

- cluster:

  the cluster

- variable:

  the variable

- value:

  the mean value of `variable` for the given cluster and interation

a ggplot2 expression.

a ggplot2 expression.

the `cv` object with the cluster labels in the `oob_sample` data frame
reassigned so that the labels match across all iterations as best as
possible.

a `ggplot2` expression.

## Details

The number of rows in the resulting data frames will be equal to:
`ncol(df) * n_samples * n_clusters`.
