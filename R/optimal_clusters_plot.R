#' @rdname fitmeasures
#' @param x the result of [clav::optimal_clusters()]
#' @param ... other parameters passed to [cowplot::plot_grid()]
#' @return a ggplot2 expression
#' @export
#' @method plot optimalclusters
#' @import ggplot2
#' @importFrom cowplot plot_grid
plot.optimalclusters <- function(x, ...) {
	plots <- list()

	x_breaks <- seq(1, max(x$k))

	if('davies_bouldin' %in% names(x)) {
		plots[['davies_bouldin']] <- ggplot(x,
											aes(x = .data$k, y = .data$davies_bouldin)) +
			geom_path() +
			geom_point() +
			scale_x_continuous(breaks = x_breaks) +
			xlab('Number of Clusters') +
			ylab("Davies-Bouldin's Index") +
			theme_minimal() +
			xlab('') + ylab('') +
			ggtitle("Davies-Bouldin's Index", subtitle = 'Lower values desired')
	}

	if('calinski_harabasz' %in% names(x)) {
		plots[['calinski']] <- ggplot(x,
									  aes(x = .data$k, y = .data$calinski_harabasz)) +
			geom_path() +
			geom_point() +
			scale_x_continuous(breaks = x_breaks) +
			xlab('Number of Clusters') +
			ylab('Calinski-Harabasz statistic') +
			theme_minimal() +
			ylab('') +
			ggtitle('Calinski-Harabasz Statistic', subtitle = 'Higher values desired')
	}

	if('wss' %in% names(x)) {
		plots[['wss']] <- ggplot(x,
								 aes(x = .data$k, y = .data$wss)) +
			geom_hline(aes(yintercept = .data$wss), linetype = 2, color = 'grey50') +
			geom_path() +
			geom_point() +
			scale_x_continuous(breaks = x_breaks) +
			xlab('Number of Clusters') +
			ylab('Within Group Sum of Squares') +
			theme_minimal() +
			xlab('') + ylab('') +
			ggtitle('Within Group Sum of Squares', subtitle = 'Elbow method')
	}

	if('silhoutte' %in% names(x)) {
		plots[['silhouette']] <- ggplot(x,
										aes(x = .data$k, y = .data$silhoutte)) +
			geom_path() +
			geom_point() +
			scale_x_continuous(breaks = x_breaks) +
			xlab('Number of Clusters') +
			ylab('Silhouette Score') +
			theme_minimal() +
			xlab('') + ylab('') +
			ggtitle('Silhouette Score', subtitle = 'Higher values desired')
	}

	if('gap' %in% names(x)) {
		plots[['gap']] <- ggplot(x,
								 aes(x = .data$k, y = .data$gap)) +
			geom_path() +
			geom_point() +
			scale_x_continuous(breaks = x_breaks) +
			xlab('Number of Clusters') +
			ylab('Gap Statistic') +
			theme_minimal() +
			xlab('') + ylab('') +
			ggtitle('Gap Statistic', subtitle = 'Higher values desired')
	}

	if('rand_index' %in% names(x)) {
		plots[['rand']] <- ggplot(x,
								 aes(x = .data$k, y = .data$rand_index)) +
			geom_path() +
			geom_point() +
			scale_x_continuous(breaks = x_breaks) +
			xlab('Number of Clusters') +
			ylab('Rand Index') +
			theme_minimal() +
			xlab('') + ylab('') +
			ggtitle('Rand Index', subtitle = 'Similarity with k - 1 model')
	}

	params <- list(...)
	for(i in seq_len(length(params))) {
		plots[[names(params)[i]]] <- params[[i]]
	}

	do.call(cowplot::plot_grid, plots)
}
