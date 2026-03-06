#' Hierarchical Clustering
#'
#' This is a wrapper function to [stats::hclust()] to coerce the call and results to conform
#' to the same structure as [stats::kmeans()].
#'
#' @param x data frame of variables to find clusters for.
#' @param k the desired number of clusters.
#' @param ... other parameters passed to [stats::hclust()].
#' @return the results of [stats::hclust()] along with additional values, namely:
#' \describe{
#'   \item{k}{the desired number of clusters.}
#'   \item{cluster}{a vector with the cluster membership.}
#' }
#' @export
#' @importFrom stats hclust cutree dist
hclust2 <- function(x, k, ...) {
	result <- stats::hclust(stats::dist(x), ...)
	result$k <- k
	result$cluster <- stats::cutree(result, k = k)
	class(result) <- c('hclust2', class(result))
	return(result)
}
