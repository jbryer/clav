# Utility function used to determine the optimal number of clusters.

Utility function used to determine the optimal number of clusters.

## Usage

``` r
optimal_clusters(df, max_k = 9)

# S3 method for class 'optimalclusters'
plot(x, ...)
```

## Arguments

- df:

  data frame to determine the optimal number of clusters from.

- max_k:

  maximun number of clusters to estimate.

- x:

  the result of `optimal_clusters()`

- ...:

  other parameters passed to
  [`cowplot::plot_grid()`](https://wilkelab.org/cowplot/reference/plot_grid.html)

## Value

a data frame with various metrics used to determine the optimal number
of clusters. Each row corresponds to k ranging from 1 to `max_k` and has
the following columns:

- k:

  number of clusters

- wss:

  within sum of squares

- silhoutte:

- gap:

- calinski_harabasz:

- davies_bouldin:

a ggplot2 expression
