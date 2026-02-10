#' Cluster Agreement Fit
#'
#' This function calculates the cluster agreement fit for varying cluster sizes (k). Note that this
#' function will call [cluster_validation()] for each k hence this function may take a while to
#' execute for large data frames and large k.
#'
#' The cluster agreement fit is a measure of how often observations are either in the same cluster
#' or not. In a perfect agreement situation the matrix returned by [cluster_agreement()] will have
#' all zeros or all ones indicating that a pair of observations are never in the same cluster or
#' always in the same cluster, respectively.
#'
#' @rdname cluster_agreement_fit
#' @param df data frame to estimate clusters. Note that all columns will be used in the estimation.
#' @param k vector indicating the number of clusters to calculate the agreement fit for.
#' @param verbose whether the function should print the status while running.
#' @param ... other parameters passed to [cluster_validation()].
#' @return a list of length `length(k)`. Each element is a list with the following elements:
#' \describe{
#' \item{k}{the number of clusters estimated.}
#' \item{cv}{the results from [cluster_validation()]}
#' \item{ca_mat}{the results from [cluster_agreement()]}
#' \item{caf}{the cluster agreement fit.}
#' }
#' @export
cluster_agreement_fit <- function(
		df,
		k = 2:6,
		verbose = interactive(),
		...) {
	caf <- list()
	for(i in k) {
		if(verbose) { message(paste0('Estimating for k = ', i)) }
		cv <- clav::cluster_validation(df = df, n_clusters = i, ...)
		ca_mat <- cluster_agreement(cv)
		caf[[length(caf) + 1]] <- list(
			k = i,
			cv = cv,
			ca_mat = ca_mat,
			caf = ca_mat[!is.na(ca_mat)] %in% c(0, 1) |> mean()
		)
	}
	names(caf) <- paste0('k', k)
	class(caf) <- c('cluster_agreement_fit', 'list')
	return(caf)
}

#' S3 plot function for [cluster_agreement_fit()].
#' @rdname cluster_agreement_fit
#' @param x results from [cluster_agreement_fit()]
#' @param palette_type One of "seq" (sequential), "div" (diverging) or "qual" (qualitative)
#' @param palette f a string, will use that named palette.
#' @param palette_direction Sets the order of colours in the scale.
#' @param ... other parmaters passed to [summary.cluster_agreement_fit()].
#' @return a ggplot2 expression.
#' @method plot cluster_agreement_fit
#' @export
plot.cluster_agreement_fit <- function(
		x,
		palette_type = 'qual',
		palette = 1,
		palette_direction = -1,
		...
) {
	df <- summary(x, ...)
	p <- ggplot(df, aes(x = k, y = fit)) +
		geom_path(aes(group = 'overall')) +
		geom_point() +
		ylim(c(0.5, 1)) +
		ylab('Cluster Agreement')
	if(ncol(df) > 2) {
		df_main <- df[,1:2]
		df_thresholds <- df[,c(1, 3:ncol(df))] |>
			reshape2::melt(id.vars = 'k')
		p <- p +
			geom_path(data = df_thresholds, aes(x = k, y = value, color = variable, group = variable)) +
			geom_point(data = df_thresholds, aes(x = k, y = value, color = variable, group = variable)) +
			scale_color_brewer('Threshold',
							   type = palette_type,
							   direction = palette_direction,
							   palette = palette)
	}
	p <- p + geom_label(aes(label = round(fit, digits = 2)), vjust = 1.5)
	return(p)
}

#' S3 hist function for [cluster_agreement_fit()].
#' @rdname cluster_agreement_fit
#' @param x results from [cluster_agreement_fit()]
#' @param exclude_agreements if `TRUE` agreements of zero or one will be excluded from the bar
#'        plot. This may be helpful for seeing the pattern of how much agreement there may be
#'        when the mode is zero and/or one.
#' @param ... currently not used.
#' @return a ggplot2 expression.
#' @method hist cluster_agreement_fit
#' @export
hist.cluster_agreement_fit <- function(x, exclude_agreements = FALSE, ...) {
	tmp <- lapply(x, function(x) {
		val <- as.numeric(x$ca_mat)
		data.frame(k = x$k,
				   caf = x$caf,
				   agree = val[!is.na(val)])
	})
	df <- do.call(rbind, tmp)
	if(exclude_agreements) {
		df <- df[!df$agree %in% c(0, 1),]
	}
	ggplot(df, aes(x = agree)) +
		geom_bar() +
		facet_wrap(~ paste0('k = ', k, '; agreement = ', round(caf, digits = 2)), ncol = 1)
}

#' S3 print function for [cluster_agreement_fit()].
#' @rdname cluster_agreement_fit
#' @param x results from [cluster_agreement_fit()]
#' @param ... currently not used.
#' @return a data.frame providing the agreement fit for each k.
#' @method print cluster_agreement_fit
#' @export
print.cluster_agreement_fit <- function(x, ...) {
	summary(x, ...) |> print()
}

#' S3 summary function for [cluster_agreement_fit()].
#' @rdname cluster_agreement_fit
#' @param object results from [cluster_agreement_fit()]
#' @param thresholds a vector of thresholds to consider "perfect" agreement. Specifically, values, x,
#'        where x < 0 + threshold or x > (1 - threshold) are considered to agree. Additional columns
#'        of the format `fitXXX` where `XXX` is the threshold.
#' @param ... currently not used.
#' @return a data.frame providing the agreement fit for each k.
#' @method summary cluster_agreement_fit
#' @export
summary.cluster_agreement_fit <- function(object, thresholds, ...) {
	df <- data.frame(
		k = 2:6,
		fit = lapply(object, FUN = function(x) { x$caf }) |> unlist()
	)
	if(!missing(thresholds)) {
		for(i in thresholds) {
			df[,paste0('Within_', i)] <- NA
		}
		for(i in row.names(df)) {
			ca_mat <- object[[i]]$ca_mat
			for(j in thresholds) {
				df[i,paste0('Within_', j)] <- (ca_mat[!is.na(ca_mat)] <= j |
										   	ca_mat[!is.na(ca_mat)] >= (1 - j)) |> mean()
			}
		}

	}
	return(df)
}

#' Cluster Agreement
#'
#' This function will calculate how the cluster membership agreement among all observations.
#' For each bootstrap sample, cluster membership is determined by using the `predict()` function
#' so that each observation has exactly one cluster ID. This also means that both in- and
#' out-of-bag samples are contributing to the fit statistic. The value in the matrix is a value
#' between zero and one indicating the proportion of times the two observations had the same
#' cluster membership.
#'
#' @rdname cluster_agreement_fit
#' @export
#' @param cv a [cluster_validation()] object.
#' @return a matrix with how often each observations cluster membership agrees with all other
#' observations across all bootstrap sample.
cluster_agreement <- function(cv) {
	df <- cv$data
	cluster_assignments <- data.frame(row.names = row.names(df))
	for(i in 1:length(cv$in_sample_model_fits)) {
		model_fit <- cv$in_sample_model_fits[[i]]
		labels <- cv$in_cluster_mapping[[i]]
		cluster_assignments[,i] <- predict(model_fit, newdata = df)
		cluster_assignments[,i] <- factor(cluster_assignments[,i],
										  levels = 1:cv$k,
										  labels = labels)
	}

	# start <- Sys.time()
	mm <- list()
	for(j in 1:ncol(cluster_assignments)) {
		m <- matrix(0, nrow = nrow(cluster_assignments), ncol = nrow(cluster_assignments))
		for(i in unique(cluster_assignments[,j])) {
			pos <- which(cluster_assignments[,j] == i)
			m[pos, pos] <- 1
		}
		mm[[j]] <- m
	}
	match_matrix <- Reduce("+", mm) / ncol(cluster_assignments)
	match_matrix[lower.tri(match_matrix, diag = TRUE)] <- NA
	# end <- Sys.time()
	# end - start  # 0.17 seconds

	# TODO: Is there a more efficient way of calculating this?
	# start <- Sys.time()
	# match_matrix <- matrix(NA, nrow = nrow(cluster_assignments), ncol = nrow(cluster_assignments))
	# for(i in 1:(nrow(match_matrix)-1)) {
	# 	row1 <- cluster_assignments[i,,drop=TRUE] |> as.integer()
	# 	for(j in (i+1):nrow(match_matrix)) {
	# 		row2 <- cluster_assignments[j,,drop=TRUE] |> as.integer()
	# 		match_matrix[i,j] <- mean(row1 == row2)
	# 	}
	# }
	# end <- Sys.time()
	# end - start  # 21 seconds

	return(match_matrix)
}
