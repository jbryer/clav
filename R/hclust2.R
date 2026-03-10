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
#' @rdname hclust2
hclust2 <- function(x, k, ...) {
	result <- stats::hclust(stats::dist(x), ...)
	result$k <- k
	result$data <- x
	result$cluster <- stats::cutree(result, k = k)
	class(result) <- c('hclust2', class(result))
	return(result)
}

#' Predict cluster membership for heirarchical clustering.
#'
#' @param object results from [hclust2()].
#' @param newdata data frame to get predicted clusters. If omitted then clusters for data used
#'        to train model will be returned.
#' @param method whether to return the class identifier or the center.
#' @param ... currently not used.
#' @return vector of predicted clusters (if `method = 'classes'`) or data frame with cluster centers
#'        (if `method = 'centers'`).
#' @export
#' @method predict hclust2
#' @rdname hclust2
predict.hclust2 <- function(
		object,
		newdata,
		method = c("classes", "centers"),
		...
) {
	method <- match.arg(method)
	if(missing(newdata)) {
		newdata <- object$data
	}
	clusters <- stats::cutree(object, k = object$k)
	centers <- get_centers(object$data, clusters)
	ss_by_center <- apply(centers, 1, function(x) {
		colSums((t(newdata) - x) ^ 2)
	})
	best_clusters <- apply(ss_by_center, 1, which.min)
	if (method == "centers") {
		centers[best_clusters, ]
	} else {
		best_clusters
	}
}
