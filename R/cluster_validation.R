#' Cluster profile validation
#'
#' This function takes multiple random samples from the provided data frame,
#' estimate cluster membership, and calculate the mean (the default, a different
#' statistic can be specified using the `summary_fun` parameter, e.g. `median`
#' may be appropriate) for each cluster using both the in sample and out-of-bag
#' (i.e. out of sample). For the out-of-bag sample
#'
#' The number of rows in the resulting data frames will be equal to:
#' `ncol(df) * n_samples * n_clusters`.
#'
#' @param df data frame to estimate clusters. Note that all columns will be used
#'        in the estimation.
#' @param n_clusters the number of clusters to estimate.
#' @param cluster_fun the function used to estimate the clusters.
#' @param get_cluster_fun the function used to get the cluster classes. This function
#'        takes one parameter, the result of `cluster_fun`.
#' @param oob_predict_fun the function used to get predictions from the out-of-bag
#'        sample. Function takes two parameters, the first is the results of
#'        `cluster_fun`, the second is the  out-of-bag sample data frame.
#' @param summary_fun the function used to calculate the statistic for each
#'        cluster and iteration. Defaults to `mean`.
#' @param n_samples the number of random samples to draw.
#' @param sample_size the size of each random sample. Defaults to 50% of observations.
#' @param replace whether sampling should be done with replacement.
#' @param standardize whether the variables should be standardized before
#'	      estimating clusters.
#' @param seed random number seed. Note that the seed is set before each iteration
#'        to `seed + i` where `i` is the iteration number.
#' @param verbose whether the function should print the status while running.
#' @param ... other parameters passed to `cluster_fun`.
#' @return a list with the following elements:
#' \describe{
#'   \item{complete_sample}{data frame of results using the entire data set.}
#'   \item{in_sample}{data frame of in sample results.}
#'   \item{oob_sample}{data frame of out-of-bag results.}
#'   \item{complete_model_fit}{model fit for the full data set.}
#'   \item{in_sample_model_fits}{model fits for each sample.}
#' }
#' Each of these data frames contain four columns:
#' \describe{
#'   \item{iter}{the iteration}
#'   \item{cluster}{the cluster}
#'   \item{variable}{the variable}
#'   \item{value}{the mean value of `variable` for the given cluster and interation}
#' }
#' @name cluster_validation
#' @export
cluster_validation <- function(
		df,
		n_clusters = 2,
		cluster_fun = kmeans,
		get_cluster_fun = function(x) { x$cluster },
		oob_predict_fun = function(fit, newdata) { predict(fit, newdata = newdata) },
		summary_fun = mean,
		n_samples = 100,
		sample_size = 0.5 * nrow(df),
		replace = FALSE,
		standardize = TRUE,
		seed,
		verbose = interactive(),
		...
) {
	if(sample_size < 30 | sample_size > nrow(df)) {
		stop(paste0('sample_size parameter should be between 30 and ', nrow(df)))
	}
	if(sample_size == nrow(df) & !replace) {
		stop('sample_size is equal to the number of observations and REPLACE = FALSE.
			 No unique random samples will be drawn.')
	}

	if(standardize) {
		scale_this <- function(x) {
			(x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
		}
		df <- df |> dplyr::mutate_if(is.numeric, scale_this)
	}

	if(verbose) {
		pb <- utils::txtProgressBar(min = 0, max = n_samples, style = 3)
	}

	if(!missing(seed)) { set.seed(seed) }
	full_fit <- cluster_fun(df, n_clusters)
	full_fit_result <- df |>
		dplyr::mutate(
			cluster = factor(get_cluster_fun(full_fit),
							 labels = LETTERS[1:n_clusters],
							 ordered = TRUE)) |>
		dplyr::group_by(.data$cluster) |>
		dplyr::summarise_at(names(df), summary_fun) |>
		reshape2::melt(id.vars = 'cluster') |>
		dplyr::mutate(variable = factor(.data$variable,
										levels = names(df),
										ordered = TRUE))

	results <- data.frame()
	results_oob <- data.frame()
	model_results <- list()
	for(i in 1:n_samples) {
		if(verbose) { utils::setTxtProgressBar(pb, i) }
		if(!missing(seed)) { set.seed(seed + i) }
		rows <- sample(nrow(df), size = sample_size, replace = replace)
		fit <- cluster_fun(df[rows,], n_clusters, ...)
		model_results[[i]] <- fit

		result <- df[rows,] |>
			dplyr::mutate(
				cluster = factor(get_cluster_fun(fit),
								 labels = LETTERS[1:n_clusters],
								 ordered = TRUE)) |>
			dplyr::group_by(.data$cluster) |>
			dplyr::summarise_at(names(df), summary_fun) |>
			reshape2::melt(id.vars = 'cluster') |>
			dplyr::mutate(iter = i) |>
			dplyr::mutate(variable = factor(.data$variable,
											levels = names(df),
											ordered = TRUE)) |>
			dplyr::relocate(iter)
		results <- rbind(results, result)

		result_oob <- df[-rows,] |>
			dplyr::mutate(
				cluster = factor(oob_predict_fun(fit, df[-rows,]),
								 labels = LETTERS[1:n_clusters],
								 ordered = TRUE)) |>
			dplyr::group_by(.data$cluster) |>
			dplyr::summarise_at(names(df), summary_fun) |>
			reshape2::melt(id.vars = 'cluster') |>
			dplyr::mutate(iter = i) |>
			dplyr::mutate(variable = factor(.data$variable,
											levels = names(df),
											ordered = TRUE)) |>
			dplyr::relocate(.data$iter)
		results_oob <- rbind(results_oob, result_oob)
	}
	if(verbose) { close(pb) }
	cv <- list(
		variables = names(df),
		complete_sample = full_fit_result,
		in_sample = results,
		oob_sample = results_oob,
		complete_model_fit = full_fit,
		in_sample_model_fits = model_results)
	cv <- fix_cluster_labels(cv, ...)
	class(cv) <- c('clustervalidation')
	attr(cv, 'standardize') <- standardize
	return(cv)
}
