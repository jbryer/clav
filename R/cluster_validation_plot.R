#' @rdname cluster_validation
#' @param x the results of `cluster_validation`.
#' @param facet whether the different samples (i.e. in sample, out-of-bag, and
#'        and complete) should be faceted.
#' @param plot_complete whether the profile line using the complete data set
#'        should be plotted.
#' @param plot_oob_sample whether the out-of-bag sample should be plotted.
#' @param point_alpha the alpha (transparency) level for points.
#' @param line_alpha the alpha (transparency) level for lines.
#' @param xlab label for the x-axis.
#' @param ylab label for the y-axis.
#' @param point_size the size of the points.
#' @param line_width width of the lines.
#' @param ... currently not used.
#' @return a ggplot2 expression.
#' @export
#' @method plot clustervalidation
plot.clustervalidation <- function(
		x,
		facet = TRUE,
		plot_complete = TRUE,
		plot_in_sample = TRUE,
		plot_oob_sample = TRUE,
		point_alpha = 0.1,
		point_size = 1,
		line_alpha = 0.1,
		line_width = 1,
		xlab = '',
		ylab = ifelse(attr(x, 'standardize'), 'Mean Standard Score', 'Mean Score'),
		...
) {
	results_is <- x$in_sample
	results_is$estimate <- 'Sample'
	results_oob <- x$oob_sample
	results_oob$estimate <- 'Out of bag sample'
	thedata <- data.frame()

	if(plot_in_sample & plot_oob_sample) {
		thedata <- rbind(results_is, results_oob)
	} else if(plot_oob_sample) {
		thedata <- results_oob
	} else if(plot_in_sample) {
		thedata <- results_is
	} else {
		stop('plot_in_sample and/or plot_oob_sample must be TRUE.')
	}
	p <- thedata |>
		dplyr::mutate(estimate = factor(.data$estimate,
										levels = c('Sample', 'Out of bag sample'))) |>
		ggplot(aes(x = .data$variable, y = .data$value, color = cluster, #color = .data$estimate,
				   group = paste0(.data$estimate, .data$iter, .data$cluster))) +
		geom_path(alpha = line_alpha, linewidth = line_width) +
		geom_point(alpha = point_alpha, size = point_size) +
		theme_minimal() +
		theme(legend.position = 'none') +
		scale_color_brewer(type = 'qual', palette = 2) +
		xlab(xlab) + ylab(ylab)

	if(facet) {
		p <- p + facet_wrap(vars(.data$estimate))
	}
	if(plot_complete) {
		p <- p + geom_path(data = cbind(x$complete_sample, estimate = 'Complete', iter = 0),
						   color = 'blue',
						   alpha = 0.5,
						   linewidth = line_width)
	}
	return(p)
}
