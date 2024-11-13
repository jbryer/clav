#' Utility function used to determine the optimal number of clusters.
#'
#' @param df data frame to determine the optimal number of clusters from.
#' @param max_k maximun number of clusters to estimate.
#' @return a data frame with various metrics used to determine the optimal number
#' of clusters. Each row corresponds to k ranging from 1 to `max_k` and has the
#' following columns:
#' \describe{
#'	\item{k}{number of clusters}
#'	\item{wss}{within sum of squares}
#'	\item{silhoutte}{}
#'	\item{gap}{}
#'	\item{calinski_harabasz}{}
#'	\item{davies_bouldin}{}
#' }
#' @export
#' @importFrom stats kmeans
#' @importFrom cluster clusGap
optimal_clusters <- function(
		df,
		max_k = 9
) {
	wgu_gap <- cluster::clusGap(
		df,
		stats::kmeans,
		K.max = max_k,
		B = 60,
		verbose = interactive()) # NOTE: This takes a while to run

	cluster_metrics <- suppressWarnings({ data.frame(
		k = 1:max_k,
		wss = wss(df, k = max_k),
		silhoutte = silhouette_score(df, k = max_k),
		gap = wgu_gap$Tab[1:max_k,'gap'],
		calinski_harabasz = calinski_harabasz(df, k = max_k),
		davies_bouldin = davies_bouldin(df, k = max_k),
		stringsAsFactors = FALSE
	) })

	class(cluster_metrics) <- c('optimalclusters', 'data.frame')
	return(cluster_metrics)
}
