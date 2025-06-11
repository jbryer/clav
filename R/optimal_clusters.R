#' Utility function used to determine the optimal number of clusters.
#'
#' This function will calculate a number of metrics used to determine the optimal number of clusters.
#' The result will be a data frame where each row corresponds to the number of clusters (k) and
#' the columns are the various metrics. An S3 plotting function is provided.
#'
#' @rdname fitmeasures
#' @param df data frame to determine the optimal number of clusters from.
#' @param max_k maximum number of clusters to estimate.
#' @param cluster_fun cluster function to use.
#' @param wss whether to calculate the within sum of squares.
#' @param silhoutte whether to calculate the Silhoutte index.
#' @param gap whether to calculate the Gap statistic.
#' @param calinski_harabasz whether to calculate the Calinski Harabasz index
#' @param davies_bouldin whether to calculate the Davies Bouldin index.
#' @param rand_index whether to calculate the Rand index.
#' @return a data frame with various metrics used to determine the optimal number
#' of clusters. Each row corresponds to k ranging from 1 to `max_k` and has the
#' following columns (if the corresponding metric was requested):
#' \describe{
#'	\item{k}{Number of clusters}
#'	\item{wss}{Within sum of squares}
#'	\item{silhoutte}{Silhouette analysis measures the quality of clustering and provides an insight
#'	                 into the separation distance between the resulting clusters. A higher
#'	                 silhouette score indicates that the object is well matched to its own cluster
#'	                 and poorly matched to neighboring clusters.}
#'	\item{gap}{The Gap Statistic compares the total within intra-cluster variation for different
#'	           values of k with their expected values under null reference distribution of the data.
#'	           The optimal k is the value that yields the largest gap statistic.}
#'	\item{calinski_harabasz}{This Index evaluates clusters based on their compactness and separation.
#'	                         The index is calculated using the ratio of between-cluster variance to
#'	                         within-cluster variance, with higher values indicate better-defined clusters}
#'	\item{davies_bouldin}{The Davies-Bouldin index is a measure of the how much separation there is
#'	                      between clusters. Lower values of the Davies-Bouldin index indicate a
#'	                      model with better separation.}
#'  \item{rand_index}{It calculates the proportion of agreement between the two clusters, considering
#'                    both the pairs of elements that are correctly assigned to the same or different
#'                    clusters. Higher values indicate greater similarity and better clustering quality.}
#' }
#' @export
#' @importFrom stats kmeans
#' @importFrom cluster clusGap
optimal_clusters <- function(
		df,
		max_k = 9,
		cluster_fun = stats::kmeans,
		wss = TRUE,
		silhoutte = TRUE,
		gap = TRUE,
		calinski_harabasz = TRUE,
		davies_bouldin = TRUE,
		rand_index = TRUE
) {
	cluster_metrics <- suppressWarnings({ data.frame(
		k = 1:max_k,
		stringsAsFactors = FALSE
	) })

	if(wss) {
		cluster_metrics$wss <- wss(df, k = max_k, cluster_fun = cluster_fun)
	}

	if(silhoutte) {
		cluster_metrics$silhoutte <- silhouette_score(df, k = max_k, cluster_fun = cluster_fun)
	}

	if(gap) {
		wgu_gap <- cluster::clusGap(
			df,
			FUNcluster = cluster_fun,
			K.max = max_k,
			B = 60,
			verbose = interactive()) # NOTE: This takes a while to run
		cluster_metrics$gap <- wgu_gap$Tab[1:max_k,'gap']

	}

	if(calinski_harabasz) {
		cluster_metrics$calinski_harabasz <- calinski_harabasz(df, k = max_k, cluster_fun = cluster_fun)
	}

	if(davies_bouldin) {
		cluster_metrics$davies_bouldin <- davies_bouldin(df, k = max_k, cluster_fun = cluster_fun)
	}

	if(rand_index) {
		cluster_metrics$rand_index <- rand_index(df, k = max_k, cluster_fun = cluster_fun)
	}

	class(cluster_metrics) <- c('optimalclusters', 'data.frame')
	return(cluster_metrics)
}
