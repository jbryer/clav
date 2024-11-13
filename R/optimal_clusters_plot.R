#' @rdname optimal_clusters
#' @param x the result of [clav::optimal_clusters()]
#' @param ... other parameters passed to [cowplot::plot_grid()]
#' @return a ggplot2 expression
#' @export
#' @method plot optimalclusters
#' @import ggplot2
#' @importFrom cowplot plot_grid
plot.optimalclusters <- function(x, ...) {
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

	params <- list(...)
	for(i in seq_len(length(params))) {
		plots[[names(params)[i]]] <- params[[i]]
	}

	do.call(cowplot::plot_grid, plots)
}
