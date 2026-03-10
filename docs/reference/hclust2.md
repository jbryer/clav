# Hierarchical Clustering

This is a wrapper function to
[`stats::hclust()`](https://rdrr.io/r/stats/hclust.html) to coerce the
call and results to conform to the same structure as
[`stats::kmeans()`](https://rdrr.io/r/stats/kmeans.html).

## Usage

``` r
hclust2(x, k, ...)

# S3 method for class 'hclust2'
predict(object, newdata, method = c("classes", "centers"), ...)
```

## Arguments

- x:

  data frame of variables to find clusters for.

- k:

  the desired number of clusters.

- ...:

  currently not used.

- object:

  results from `hclust2()`.

- newdata:

  data frame to get predicted clusters. If omitted then clusters for
  data used to train model will be returned.

- method:

  whether to return the class identifier or the center.

## Value

the results of [`stats::hclust()`](https://rdrr.io/r/stats/hclust.html)
along with additional values, namely:

- k:

  the desired number of clusters.

- cluster:

  a vector with the cluster membership.

vector of predicted clusters (if `method = 'classes'`) or data frame
with cluster centers (if `method = 'centers'`).
