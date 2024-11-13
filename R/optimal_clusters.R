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

#' @rdname optimal_clusters
#' @param x the result of [clav::optimal_clusters()]
#' @param ... other parameters passed to [cowplot::plot_grid()]
#' @return a ggplot2 expression
#' @export
#' @method plot optimalclusters
#' @import ggplot2
#' @importFrom cowplot plot_grid
plot.optimalclusters <- function(
		x,
		...
) {
	plots <- list()

	plots[['davies_bouldin']] <- ggplot(x,
										aes(x = .data$k, y = .data$davies_bouldin)) +
		geom_path() +
		geom_point() +
		scale_x_continuous(breaks = 1:9) +
		xlab('Number of Clusters') +
		ylab("Davies-Bouldin's Index") +
		theme_minimal() +
		xlab('') + ylab('') +
		ggtitle("Davies-Bouldin's Index", subtitle = 'Lower values desired')

	plots[['calinski']] <- ggplot(x,
								  aes(x = .data$k, y = .data$calinski_harabasz)) +
		geom_path() +
		geom_point() +
		scale_x_continuous(breaks = 1:9) +
		xlab('Number of Clusters') +
		ylab('Calinski-Harabasz statistic') +
		theme_minimal() +
		ylab('') +
		ggtitle('Calinski-Harabasz Statistic', subtitle = 'Higher values desired')

	plots[['wss']] <- ggplot(x,
							 aes(x = .data$k, y = .data$wss)) +
		geom_hline(aes(yintercept = .data$wss), linetype = 2, color = 'grey50') +
		geom_path() +
		geom_point() +
		scale_x_continuous(breaks = 1:9) +
		xlab('Number of Clusters') +
		ylab('Within Group Sum of Squares') +
		theme_minimal() +
		xlab('') + ylab('') +
		ggtitle('Within Group Sum of Squares', subtitle = 'Elbow method')

	plots[['silhouette']] <- ggplot(x,
									aes(x = .data$k, y = .data$silhoutte)) +
		geom_path() +
		geom_point() +
		scale_x_continuous(breaks = 1:9) +
		xlab('Number of Clusters') +
		ylab('Silhouette Score') +
		theme_minimal() +
		xlab('') + ylab('') +
		ggtitle('Silhouette Score', subtitle = 'Higher values desired')

	plots[['gap']] <- ggplot(x,
							 aes(x = .data$k, y = .data$gap)) +
		geom_path() +
		geom_point() +
		scale_x_continuous(breaks = 1:9) +
		xlab('Number of Clusters') +
		ylab('Gap Statistic') +
		theme_minimal() +
		xlab('') + ylab('') +
		ggtitle('Gap Statistic', subtitle = 'Higher values desired')

	do.call(cowplot::plot_grid, plots, ...)
}
