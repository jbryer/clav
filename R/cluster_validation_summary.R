#' @rdname cluster_validation
#' @param object the results of `cluster_validation`.
#' @param in_sample plot the in sample results.
#' @param oob_sample plot the out-of-bag sample results.
#' @param ... currently not used.
#' @method summary clustervalidation
#' @export
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

	mapping <- samp |> reshape2::dcast(
		iter + cluster ~ full_cluster, fun.aggregate = length
	)
	# mapping <- samp |> reshape2::dcast(.data$iter + .data$cluster ~ .data$full_cluster,
	# 								   fun.aggregate = length)

	mapping$full_cluster <- apply(mapping[,cluster_labs], 1, FUN = function(x) {
		cluster_labs[which(x == max(x))][1]
	})

	samp <- samp |>
		# dplyr::select(.data$variable, .data$iter, .data$cluster, .data$value) |>
		dplyr::select(variable, iter, cluster, value) |>
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
#' @export
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
