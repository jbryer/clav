# Function to perform K-means step by step and save iterations

This function performs K-means clustering but saves the cluster
assignment and centers for each iteration of the algorithm.

## Usage

``` r
kmeans_iterative(x, centers, max_iter = 50)
```

## Arguments

- x:

  numeric matrix of data, or an object that can be coerced to such a
  matrix (such as a numeric vector or a data frame with all numeric
  columns).

- centers:

  either a numeric value for k or a k by ncol(x) matrix of the initial
  centers.

- max_iter:

  maximum number of iterations before stopping if convergence doesn't
  occur first.

## Value

a list where each element corresponds to an interation in the k-means
algorithm. For each element there are three values: `iteration` (numeric
for the which iteration), `centers` (matrix for the cluster centers),
and `clusters` (vector of cluster assignment for all observations in
`x`).
