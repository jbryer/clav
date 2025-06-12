#' @rdname cluster_validation
#' @param x the results of `cluster_validation`.
#' @param plot_complete whether the profile line using the complete data set
#'        should be plotted.
#' @param plot_oob_sample whether the out-of-bag sample should be plotted.
#' @param point_alpha the alpha (transparency) level for points.
#' @param line_alpha the alpha (transparency) level for lines.
#' @param xlab label for the x-axis.
#' @param ylab label for the y-axis.
#' @param point_size the size of the points.
#' @param line_width width of the lines.
#' @param complete_color the color of the path for the path using the complete dataset
#'        (i.e. `plot_complete = TRUE`)
#' @param complete_size the size of the path for the path using the complete dataset
#'        (i.e. `plot_complete = TRUE`)
#' @param complete_point_size the point size of the path for the path using the complete dataset
#'        (i.e. `plot_complete = TRUE`)
#' @param ... currently not used.
#' @return a ggplot2 expression.
#' @export
#' @method plot clustervalidation
plot.clustervalidation <- function(
		x,
		plot_complete = TRUE,
		plot_in_sample = TRUE,
		plot_oob_sample = TRUE,
		point_alpha = 0.1,
		point_size = 1,
		line_alpha = 0.1,
		line_width = 1,
		complete_color = 'blue',
		complete_size = 1,
		complete_point_size = 2,
		xlab = '',
		ylab = ifelse(attr(x, 'standardize'), 'Mean Standard Score', 'Mean Score'),
		...
) {
	p <- NULL

	ylim <- range(c(x$in_sample$mean, x$oob_sample$mean))

	complete_sample <- x$complete_sample |> dplyr::arrange(variable)

	if(plot_in_sample) {
		p_is <- x$in_sample |>
			dplyr::arrange(variable) |>
			ggplot(aes(x = .data$variable,
					   y = .data$mean,
					   color = .data$cluster,
					   group = paste0(.data$iter, .data$cluster))) +
			geom_path(alpha = line_alpha, linewidth = line_width) +
			geom_point(alpha = point_alpha, size = point_size) +
			theme_minimal() +
			# scale_x_discrete(breaks = levels(x$in_sample$variable)) +
			theme(legend.position = 'none') +
			scale_color_brewer(type = 'qual', palette = 2) +
			xlab(xlab) + ylab(ylab) +
			ylim(ylim) +
			ggtitle('Training sample')
		if(plot_complete) {
			p_is <- p_is +
				geom_path(data = complete_sample,
						  aes(group = cluster),
						  color = complete_color,
						  size = complete_size) +
				geom_point(data = complete_sample,
						   aes(group = cluster),
						   color = complete_color,
						   size = complete_point_size)
		}
	}

	if(plot_oob_sample) {
		p_oob <- x$oob_sample |>
			ggplot(aes(x = .data$variable,
					   y = .data$mean,
					   color = .data$cluster,
					   group = paste0(.data$iter, .data$cluster))) +
			geom_path(alpha = line_alpha, linewidth = line_width) +
			geom_point(alpha = point_alpha, size = point_size) +
			theme_minimal() +
			theme(legend.position = 'none') +
			scale_color_brewer(type = 'qual', palette = 2) +
			xlab(xlab) + ylab(ylab) +
			ylim(ylim) +
			ggtitle('Out-of-bag sample')
		if(plot_complete) {
			p_oob <- p_oob +
				geom_path(data = complete_sample,
						  aes(group = cluster),
						  color = complete_color,
						  size = complete_size) +
				geom_point(data = complete_sample,
						   aes(group = cluster),
						   color = complete_color,
						   size = complete_point_size)
		}
	}
	if(plot_in_sample & plot_oob_sample) {
		p <- cowplot::plot_grid(p_is, p_oob)
	} else if(plot_oob_sample) {
		p <- p_is
	} else if(plot_in_sample) {
		p <- p_oob
	} else {
		stop('plot_in_sample and/or plot_oob_sample must be TRUE.')
	}

	return(p)
}
