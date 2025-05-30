#' Fix cluster labels so they match across all samples
#'
#' This will look at the first iteration and sort the cluster assignment. The cluster labels for the
#' remaining iterations will be updated to match the first iteration by sorting the value of
#' the `var` variable. By default it will use the variable with the largest median differences
#' between adjacent clusters but for best results select a variable that has the greatest
#' separation between cluster values.
#'
#' @rdname cluster_validation
#' @param cv the results from [cluster_validation()].
#' @param var the variable name used to match cluster labels.
#' @param ... currently not used.
#' @return the `cv` object with the cluster labels in the `oob_sample` data frame reassigned so
#'         that the labels match across all iterations as best as possible.
#' @importFrom dplyr arrange pull
fix_cluster_labels <- function(cv, var,  ...) {
	if(missing(var)) {
		diffs <- sapply(
			unique(cv$complete_sample$variable),
			FUN = function(x) {
				cv$complete_sample[cv$complete_sample$variable == x,]$value |> sort() |> diff() |> median()
			}
		)
		names(diffs) <- unique(cv$complete_sample$variable)
		var <- names(diffs)[diffs == max(diffs)]
	}

	oob <- cv$oob_sample
	is <- cv$in_sample
	base <- cv$complete_sample
	base_order <- base[base$variable == var,] |>
		dplyr::arrange(dplyr::desc(value)) |>
		dplyr::pull(cluster) |>
		as.character()
	clusters <- unique(oob$cluster) |> as.character()
	n_inters <- max(oob$iter)

	# Fix in sample
	for(i in 1:n_inters) {
		comp <- is[is$iter == i,]
		comp_order <- comp[comp$variable == var,] |>
			dplyr::arrange(dplyr::desc(value)) |>
			dplyr::pull(cluster) |>
			as.character()
		rows <- list()
		for(j in 1:length(clusters)) {
			rows[[j]] <- which(is$iter == i & is$cluster == comp_order[j])
		}
		for(j in 1:length(clusters)) {
			is[rows[[j]],]$cluster <- base_order[j]
		}
	}

	# Fix out-of-bag
	for(i in 1:n_inters) {
		comp <- oob[oob$iter == i,]
		comp_order <- comp[comp$variable == var,] |>
			dplyr::arrange(dplyr::desc(value)) |>
			dplyr::pull(cluster) |>
			as.character()
		rows <- list()
		for(j in 1:length(clusters)) {
			rows[[j]] <- which(oob$iter == i & oob$cluster == comp_order[j])
		}
		for(j in 1:length(clusters)) {
			oob[rows[[j]],]$cluster <- base_order[j]
		}
	}

	cv$in_sample <- is
	cv$oob_sample <- oob
	return(cv)
}
