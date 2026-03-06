# Hierarchical Clustering

This is a wrapper function to
[`stats::hclust()`](https://rdrr.io/r/stats/hclust.html) to coerce the
call and results to conform to the same structure as
[`stats::kmeans()`](https://rdrr.io/r/stats/kmeans.html).

## Usage

``` r
hclust2(x, k, ...)
```

## Arguments

- x:

  data frame of variables to find clusters for.

- k:

  the desired number of clusters.

- ...:

  other parameters passed to
  [`stats::hclust()`](https://rdrr.io/r/stats/hclust.html).

## Value

the results of [`stats::hclust()`](https://rdrr.io/r/stats/hclust.html)
along with additional values, namely:

- k:

  the desired number of clusters.

- cluster:

  a vector with the cluster membership.
