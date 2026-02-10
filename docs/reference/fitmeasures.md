# Utility function used to determine the optimal number of clusters.

This function will calculate a number of metrics used to determine the
optimal number of clusters. The result will be a data frame where each
row corresponds to the number of clusters (k) and the columns are the
various metrics. An S3 plotting function is provided.

## Usage

``` r
optimal_clusters(
  df,
  max_k = 9,
  cluster_fun = stats::kmeans,
  wss = TRUE,
  silhoutte = TRUE,
  gap = TRUE,
  calinski_harabasz = TRUE,
  davies_bouldin = TRUE,
  rand_index = TRUE
)

# S3 method for class 'optimalclusters'
plot(x, ...)

wss(df, k = 9, cluster_fun = stats::kmeans)

silhouette_score(df, k = 9, cluster_fun = stats::kmeans, ...)

calinski_harabasz(df, k = 9, cluster_fun = stats::kmeans, ...)

davies_bouldin(df, k = 9, cluster_fun = stats::kmeans, ...)

rand_index(df, k = 9, cluster_fun = stats::kmeans, ...)
```

## Arguments

- df:

  data frame to calculate the Rand index from.

- max_k:

  maximum number of clusters to estimate.

- cluster_fun:

  clustering function.

- wss:

  whether to calculate the within sum of squares.

- silhoutte:

  whether to calculate the Silhoutte index.

- gap:

  whether to calculate the Gap statistic.

- calinski_harabasz:

  whether to calculate the Calinski Harabasz index

- davies_bouldin:

  whether to calculate the Davies Bouldin index.

- rand_index:

  whether to calculate the Rand index.

- x:

  the result of `optimal_clusters()`

- ...:

  other parameters passed to
  [`fossil::rand.index()`](https://rdrr.io/pkg/fossil/man/rand.index.html).

- k:

  number of clusters.

## Value

a data frame with various metrics used to determine the optimal number
of clusters. Each row corresponds to k ranging from 1 to `max_k` and has
the following columns (if the corresponding metric was requested):

- k:

  Number of clusters

- wss:

  Within sum of squares

- silhoutte:

  Silhouette analysis measures the quality of clustering and provides an
  insight into the separation distance between the resulting clusters. A
  higher silhouette score indicates that the object is well matched to
  its own cluster and poorly matched to neighboring clusters.

- gap:

  The Gap Statistic compares the total within intra-cluster variation
  for different values of k with their expected values under null
  reference distribution of the data. The optimal k is the value that
  yields the largest gap statistic.

- calinski_harabasz:

  This Index evaluates clusters based on their compactness and
  separation. The index is calculated using the ratio of between-cluster
  variance to within-cluster variance, with higher values indicate
  better-defined clusters

- davies_bouldin:

  The Davies-Bouldin index is a measure of the how much separation there
  is between clusters. Lower values of the Davies-Bouldin index indicate
  a model with better separation.

- rand_index:

  It calculates the proportion of agreement between the two clusters,
  considering both the pairs of elements that are correctly assigned to
  the same or different clusters. Higher values indicate greater
  similarity and better clustering quality.

a ggplot2 expression
