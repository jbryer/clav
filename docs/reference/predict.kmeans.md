# Predict cluster class for k-means cluster

Predict cluster class for k-means cluster

## Usage

``` r
# S3 method for class 'kmeans'
predict(object, newdata, method = c("classes", "centers"), ...)
```

## Arguments

- object:

  results of [`stats::kmeans()`](https://rdrr.io/r/stats/kmeans.html).

- newdata:

  data frame to make class predictions.

- method:

  method used to make predictions.

- ...:

  currently not used.

## Value

vector of predicted clusters.
