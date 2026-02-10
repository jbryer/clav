# Cluster analysis fit statistic using bootstrap distributions

Note: This will always use bootstrapping.

## Usage

``` r
overlap_fit(df, k = 2:6, verbose = interactive(), ...)

# S3 method for class 'overlap'
plot(x, se_factor = 1, ...)
```

## Arguments

- df:

  data frame containing the variables to use for cluster.

- k:

  an integer vector for the cluster sizes to estimate the overlap for.

- verbose:

  whether to print status as the boostrap samples are estimated.

- ...:

  currently not used.

- x:

  results from `overlap_fit()`.

- se_factor:

  factor to multiple the standard error. For example, for a 95%
  confidence interval set `se_factor = 1.96`.

## Value

an object that inherits from a data.frame with the following columns:

- variable:

  the variable name.

- C1:

  the label for cluster 1.

- C2:

  the label for cluster 2.

- overlap:

  how much of the bootstrap distributions overlap from the distribution
  of cluster centers for clusters 1 and 2 for `variable`.

- k:

  the number of clusters.

There is a custom
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) function for
the returned object.

a `ggplot2` expression.

## Examples

``` r
if (FALSE) { # \dontrun{
data(daacs, package='clav')
cluster_vars <- c("Motivation", "Metacognition", "Strategies", "Mathematics", "Reading", "Writing")
overlap_fit <- overlap_fit(daacs[,cluster_vars])
plot(overlap_fit)
} # }
```
