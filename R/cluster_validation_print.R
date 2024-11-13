#' @rdname cluster_validation
#' @param x the results of `cluster_validation`.
#' @param ... currently not used.
#' @method print clustervalidation
#' @export
print.clustervalidation <- function(x, ...) {
	cat(paste0(
		length(unique(x$in_sample$iter)), ' random samples estimated.\n',
		'Correlation between in sample and out-of-bag sample means for each\n',
		'cluster and variable is ', round(cor(x$in_sample$value, x$oob_sample$value), digits = 3)
	))
}
