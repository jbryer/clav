#' Plot bootstrap distributions of clusters by variable.
#'
#' @rdname cluster_validation
#' @param cv the results from [cluster_validation()].
#' @param plot_in_sample whether to plot the in sample (i.e. bootstrap) distributions.
#' @param plot_oob_sample whether to plot the out-of-bag samples.
#' @param nrow number of rows. This is ignored if `plot_in_sample = TRUE` and `plot_oob_sample = TRUE`.
#' @param palette If a string, will use that named palette. If a number, will index into the list of
#'        palettes of appropriate type. See [ggplot2::scale_color_brewer()] for more information.
#' @param ... currently not used.
#' @return a `ggplot2` expression.
#' @export
#' @import ggplot2
#' @importFrom psych describeBy
plot_distributions <- function(
		cv,
		plot_in_sample = TRUE,
		plot_oob_sample = FALSE,
		nrow = NULL,
		palette = 2,
		...
) {
	gg_base <- function(df, nrow = nrow) {
		tab_out <- psych::describeBy(df$mean,
									 group = list(df$variable, df$cluster),
									 mat = TRUE, skew = FALSE)
		names(tab_out)[2:3] <- c('variable', 'cluster')
		tab_out$variable <- factor(tab_out$variable, levels = cv$variables, ordered = TRUE)
		ggplot(df, aes(x = mean, color = cluster, fill = cluster)) +
			geom_density(alpha = 0.5) +
			geom_point(data = tab_out, aes(x = mean, y = 0), size = 3) +
			facet_wrap(~ variable, nrow = nrow) +
			scale_fill_brewer(type = 'qual', palette = palette) +
			scale_color_brewer(type = 'qual', palette = palette) +
			ylab('Density')# + theme(legend.position = 'none')
	}

	p <- NULL
	if(plot_in_sample & plot_oob_sample) {
		p_in <- gg_base(cv$in_sample, nrow = 1) +
			ggtitle('Distribution of mean values from training samples',
					subtitle = paste0('k = ', length(unique(cv$complete_sample$cluster))))
		p_oob <- gg_base(cv$oob_sample, nrow = 1) +
			ggtitle('Distribution of mean values from out-of-bag samples')
		p <- cowplot::plot_grid(p_in, p_oob, ncol = 1)
	} else if(plot_in_sample) {
		p <- gg_base(cv$in_sample, nrow = nrow) +
			ggtitle('Distribution of mean values from training samples',
					subtitle = paste0('k = ', length(unique(cv$complete_sample$cluster))))
	} else if(plot_oob_sample) {
		p <- gg_base(cv$oob_sample, nrow = nrow) +
			ggtitle('Distribution of mean values from out-of-bag samples',
					subtitle = paste0('k = ', length(unique(cv$complete_sample$cluster))))
	} else {
		stop('Either plot_in_sample or plot_oob_sample (or both) must be TRUE.')
	}
	return(p)
}
