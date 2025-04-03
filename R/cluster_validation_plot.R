#' @rdname cluster_validation
#' @param x the results of `cluster_validation`.
#' @param facet whether the different samples (i.e. in sample, out-of-bag, and
#'        and complete) should be faceted.
#' @param plot_complete whether the profile line using the complete data set
#'        should be plotted.
#' @param plot_oob whether the out-of-bag sample should be plotted.
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
		plot_oob = TRUE,
		point_alpha = 0.1,
		point_size = 1,
		line_alpha = 0.1,
		line_width = 1,
		xlab = '',
		ylab = ifelse(attr(x, 'standardize'), 'Mean Standard Score', 'Mean Score'),
		...
) {
	results <- x$in_sample
	results$estimate <- 'Sample'
	results_oob <- x$oob_sample
	results_oob$estimate <- 'Out of bag sample'
	thedata <- results
	if(plot_oob) {
		thedata <- rbind(thedata, results_oob)
	}
	p <- rbind(results, results_oob) |>
		dplyr::mutate(estimate = factor(.data$estimate,
										levels = c('Sample', 'Out of bag sample'),
										ordered = TRUE)) |>
		ggplot(aes(x = .data$variable, y = .data$value, color = .data$estimate,
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
