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
			dplyr::relocate(.data$iter)
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
	cv <- list(complete_sample = full_fit_result,
			   in_sample = results,
			   oob_sample = results_oob,
			   complete_model_fit = full_fit,
			   in_sample_model_fits = model_results)
	class(cv) <- c('clustervalidation')
	attr(cv, 'standardize') <- standardize
	return(cv)
}

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
		line_alpha = 0.1,
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
		geom_path(alpha = line_alpha) +
		geom_point(alpha = point_alpha) +
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
						   linewidth = 2)
	}
	return(p)
}

#' @rdname cluster_validation
#' @param object the results of `cluster_validation`.
#' @param in_sample plot the in sample results.
#' @param oob_sample plot the out-of-bag sample results.
#' @param ... currently not used.
summary.clustervalidation <- function(
		object,
		in_sample = FALSE,
		oob_sample = TRUE,
		...
) {
	df <- data.frame()
	if(in_sample) {
		df <- rbind(df, object$in_sample)
	}
	if(oob_sample) {
		df <- rbind(df, object$oob_sample)
	}

	full <- object$complete_sample |>
		reshape2::dcast(variable ~ cluster)
	names(full)[2:ncol(full)] <- paste0('full_', names(full)[2:ncol(full)])

	cluster_labs <- unique(object$complete_sample$cluster) |> as.character()
	samp <- merge(df, full, by = 'variable', all.x = TRUE)
	for(i in unique(object$complete_sample$cluster)) {
		samp[,paste0('diff_', i)] <- abs(
			samp$value - samp[,paste0('full_', i)]
		)
	}
	samp$full_cluster <- apply(samp[,paste0('diff_', cluster_labs)], 1, FUN = function(x) {
		cluster_labs[which(x == min(x))]
	})

	mapping <- samp |> reshape2::dcast(.data$iter + .data$cluster ~ .data$full_cluster,
									   fun.aggregate = length)
	mapping$full_cluster <- apply(mapping[,cluster_labs], 1, FUN = function(x) {
		cluster_labs[which(x == max(x))][1]
	})

	samp <- samp |>
		dplyr::select(.data$variable, .data$iter, .data$cluster, .data$value) |>
		merge(mapping[,c('iter', 'cluster', 'full_cluster')],
			  by = c('iter', 'cluster'), all.x = TRUE)

	sum_tab <- psych::describeBy(
		samp$value,
		group = list(samp$full_cluster, samp$variable),
		mat = TRUE, skew = FALSE)
	names(sum_tab)[2:3] <- c('cluster', 'variable')
	sum_tab$vars <- NULL
	sum_tab$item <- NULL
	sum_tab$n <- NULL
	row.names(sum_tab) <- 1:nrow(sum_tab)
	sum_tab <- sum_tab[order(sum_tab$cluster),]
	sum_tab$cluster <- factor(sum_tab$cluster, levels = cluster_labs, ordered = TRUE)
	sum_tab$variable <- factor(sum_tab$variable, levels = unique(sum_tab$variable), ordered = TRUE)

	class(sum_tab) <- c('cv_summary', 'data.frame')
	return(sum_tab)
}

#' @rdname cluster_validation
#' @param x result of `summary.clustervalidation()`
#' @param ... currently not used.
#' @return a ggplot2 expression.
#' @method plot cv_summary
plot.cv_summary <- function(x, ...) {
	ggplot(x, aes(x = .data$variable, y = .data$mean, group = .data$cluster, color = .data$cluster)) +
		geom_path() +
		geom_errorbar(aes(ymin = .data$mean - 1.96 * .data$se,
						  ymax = .data$mean + 1.96 * .data$se), width = 0.5) +
		geom_point() +
		scale_color_brewer(type = 'qual', palette = 2) +
		theme_minimal() +
		theme(legend.position = 'bottom') +
		xlab('')
}
