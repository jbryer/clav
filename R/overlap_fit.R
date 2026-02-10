#' Cluster analysis fit statistic using bootstrap distributions
#'
#'
#' Note: This will always use bootstrapping.
#'
#' @rdname overlap_fit
#' @param df data frame containing the variables to use for cluster.
#' @param k an integer vector for the cluster sizes to estimate the overlap for.
#' @param verbose whether to print status as the boostrap samples are estimated.
#' @param ... other parameters passed to [clav::cluster_validation()].
#' @export
#' @return an object that inherits from a data.frame with the following columns:
#' \describe{
#'	 \item{variable}{the variable name.}
#'	 \item{C1}{the label for cluster 1.}
#'	 \item{C2}{the label for cluster 2.}
#'	 \item{overlap}{how much of the bootstrap distributions overlap from the distribution of cluster
#'	               centers for clusters 1 and 2 for `variable`.}
#'	 \item{k}{the number of clusters.}
#' }
#' There is a custom `plot()` function for the returned object.
#' @examples
#' \dontrun{
#' data(daacs, package='clav')
#' cluster_vars <- c("Motivation", "Metacognition", "Strategies", "Mathematics", "Reading", "Writing")
#' overlap_fit <- overlap_fit(daacs[,cluster_vars])
#' plot(overlap_fit)
#' }
overlap_fit <- function(
		df,
		k = 2:6,
		verbose = interactive(),
		...
) {
	overlap <- data.frame()
	for(i in k) {
		if(verbose) { message(paste0('Estimating for k = ', i)) }
		cv <- clav::cluster_validation(df = df,
									   n_clusters = i,
									   sample_size = nrow(df),
									   replace = TRUE,
									   verbose = verbose,
									   ...)
		overlap <- rbind(overlap, cv$overlap)
	}
	class(overlap) <- c('overlap', 'data.frame')
	return(overlap)
}

utils::globalVariables(c("agree", "fit", "k", "mean_overlap", "median_overlap", "se_overlap"))

#' Plotting function for overlap fit metrics.
#' @rdname overlap_fit
#' @param x results from [clav::overlap_fit()].
#' @param se_factor factor to multiple the standard error. For example, for a 95% confidence interval
#'        set `se_factor = 1.96`.
#' @param ... currently not used.
#' @return a `ggplot2` expression.
#' @method plot overlap
#' @export
plot.overlap <- function(x, se_factor = 1, ...) {
	overlap <- x
	overlap_sum <- overlap |>
		dplyr::group_by(k) |>
		dplyr::summarize(mean_overlap = mean(overlap),
						 median_overlap = median(overlap),
						 min_overlap = min(overlap),
						 max_overlap = max(overlap),
						 se_overlap = sd(overlap))
	overlap_sum2 <- overlap_sum |>
		dplyr::select(k, mean_overlap, median_overlap) |>
		dplyr::rename(`Mean overlap` = mean_overlap, `Median overlap` = median_overlap) |>
		reshape2::melt(id.vars = c('k'))

	ggplot(overlap_sum, aes(x = k, y = mean_overlap)) +
		geom_errorbar(aes(ymin = max(mean_overlap - se_factor * se_overlap, 0),
						  ymax = mean_overlap + se_factor * se_overlap),
					  width = 0.5, color = 'grey50') +
		geom_point(data = overlap, aes(x = k, y = overlap, color = variable)) +
		geom_path(data = overlap_sum2, aes(x = k, y = value, color = variable)) +
		geom_point(data = overlap_sum2, aes(x = k, y = value, color = variable), size = 3) +
		scale_color_brewer('', type = 'qual', palette = 2) +
		ylab('Percent Overlap')
}
