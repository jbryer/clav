# Cluster Agreement Fit

This function calculates the cluster agreement fit for varying cluster
sizes (k). Note that this function will call
[`cluster_validation()`](cluster_validation.md) for each k hence this
function may take a while to execute for large data frames and large k.

This function will calculate how the cluster membership agreement among
all observations. For each bootstrap sample, cluster membership is
determined by using the
[`predict()`](https://rdrr.io/r/stats/predict.html) function so that
each observation has exactly one cluster ID. This also means that both
in- and out-of-bag samples are contributing to the fit statistic. The
value in the matrix is a value between zero and one indicating the
proportion of times the two observations had the same cluster
membership.

## Usage

``` r
cluster_agreement_fit(df, k = 2:6, verbose = interactive(), ...)

# S3 method for class 'cluster_agreement_fit'
plot(x, palette_type = "qual", palette = 1, palette_direction = -1, ...)

# S3 method for class 'cluster_agreement_fit'
hist(x, exclude_agreements = FALSE, ...)

# S3 method for class 'cluster_agreement_fit'
print(x, ...)

# S3 method for class 'cluster_agreement_fit'
summary(object, thresholds, ...)

cluster_agreement(cv)
```

## Arguments

- df:

  data frame to estimate clusters. Note that all columns will be used in
  the estimation.

- k:

  vector indicating the number of clusters to calculate the agreement
  fit for.

- verbose:

  whether the function should print the status while running.

- ...:

  currently not used.

- x:

  results from `cluster_agreement_fit()`

- palette_type:

  One of "seq" (sequential), "div" (diverging) or "qual" (qualitative)

- palette:

  f a string, will use that named palette.

- palette_direction:

  Sets the order of colours in the scale.

- exclude_agreements:

  if `TRUE` agreements of zero or one will be excluded from the bar
  plot. This may be helpful for seeing the pattern of how much agreement
  there may be when the mode is zero and/or one.

- object:

  results from `cluster_agreement_fit()`

- thresholds:

  a vector of thresholds to consider "perfect" agreement. Specifically,
  values, x, where x \< 0 + threshold or x \> (1 - threshold) are
  considered to agree. Additional columns of the format `fitXXX` where
  `XXX` is the threshold.

- cv:

  a [`cluster_validation()`](cluster_validation.md) object.

## Value

a list of length `length(k)`. Each element is a list with the following
elements:

- k:

  the number of clusters estimated.

- cv:

  the results from [`cluster_validation()`](cluster_validation.md)

- ca_mat:

  the results from `cluster_agreement()`

- caf:

  the cluster agreement fit.

a ggplot2 expression.

a ggplot2 expression.

a data.frame providing the agreement fit for each k.

a data.frame providing the agreement fit for each k.

a matrix with how often each observations cluster membership agrees with
all other observations across all bootstrap sample.

## Details

The cluster agreement fit is a measure of how often observations are
either in the same cluster or not. In a perfect agreement situation the
matrix returned by `cluster_agreement()` will have all zeros or all ones
indicating that a pair of observations are never in the same cluster or
always in the same cluster, respectively.
