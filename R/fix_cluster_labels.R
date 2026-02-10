#' Fix cluster labels so they match across all samples
#'
#' This function will re-assign cluster labels such that the mean absolute difference to the the
#' cluster labels from the full dataset is as small as possible.
#'
#' @rdname cluster_validation
#' @param cv the results from [cluster_validation()].
#' @param greedy if `FALSE` this will find the optimal cluster pattern by calculating difference
#'        for all combinations. If `TRUE` this will consider only one variable (the one with the
#'        largest variance across means).
#' @param var the variable to use if `greedy = TRUE`.
#' @param ... currently not used.
#' @return the `cv` object with the cluster labels in the `oob_sample` data frame reassigned so
#'         that the labels match across all iterations as best as possible.
#' @importFrom dplyr arrange pull
#' @importFrom combinat permn
#' @importFrom utils menu
fix_cluster_labels <- function(
		cv,
		greedy = (length(unique(cv$oob_sample$cluster)) > 6),
		var,
		...
) {
	oob <- cv$oob_sample
	is <- cv$in_sample
	base <- cv$complete_sample
	base_means <- reshape2::dcast(base, cluster ~ variable, value.var = 'mean')
	row.names(base_means) <- base_means$cluster
	# clusters <- unique(oob$cluster) |> as.character()
	clusters <- LETTERS[1:cv$k]
	n_inters <- max(oob$iter)
	perms <- combinat::permn(clusters)

	if(length(clusters) > 6 & greedy & interactive()) {
		ans <- utils::menu(
			choices = c('Yes', 'No'),
			title = paste0('With ', length(clusters), ' clusters there will be ',
						   length(perms), ' permutations to calculate. This will take a long time.\n',
						   'Do you wish to continue with trying to align the cluster labels? ',
						   'If no, then a greedy algorithm will be used that many not perfectly ',
						   'align cluster labels.'))
		if(ans == 2) {
			greedy <- TRUE
		}
	}

	if(greedy) {
		if(missing(var)) {
			diffs <- sapply(
				unique(cv$complete_sample$variable),
				FUN = function(x) {
					cv$complete_sample[cv$complete_sample$variable == x,]$mean |>
						sort() |>
						diff() |>
						median()
				}
			)
			names(diffs) <- unique(cv$complete_sample$variable)
			var <- names(diffs)[diffs == max(diffs)]
		}

		base_order <- base[base$variable == var,] |>
			dplyr::arrange(dplyr::desc(mean)) |>
			dplyr::pull(cluster) |>
			as.character()

		# Fix in sample
		for(i in 1:n_inters) {
			comp <- is[is$iter == i,]
			comp_order <- comp[comp$variable == var,] |>
				dplyr::arrange(dplyr::desc(mean)) |>
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
				dplyr::arrange(dplyr::desc(mean)) |>
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
	} else {
		in_cluster_mapping <- list()
		oob_cluster_mapping <- list()

		# Fix in sample
		for(i in 1:n_inters) { # TODO: This is something that could be run in parallel to speed up
			comp <- is[is$iter == i,]
			comp_means <- reshape2::dcast(comp, cluster ~ variable, value.var = 'mean')
			row.names(comp_means) <- comp_means$cluster
			dists <- sapply(perms, FUN = function(x) {
				mean(as.matrix(abs(base_means[,-1] - comp_means[x,-1])))
			})
			smallest <- which(dists == min(dists))
			if(length(smallest) > 1) { smallest <- smallest[1] }
			comp_order <- perms[[smallest]]

			in_cluster_mapping[[i]] <- comp_order

			rows <- list()
			for(j in 1:length(clusters)) {
				rows[[j]] <- which(is$iter == i & is$cluster == comp_order[j])
			}
			for(j in 1:length(clusters)) {
				is[rows[[j]],]$cluster <- clusters[j]
			}
		}

		# Fix out-of-bag
		for(i in 1:n_inters) {
			comp <- oob[oob$iter == i,]
			comp_means <- reshape2::dcast(comp, cluster ~ variable, value.var = 'mean')
			row.names(comp_means) <- comp_means$cluster
			dists <- sapply(perms, FUN = function(x) {
				mean(as.matrix(abs(base_means[,-1] - comp_means[x,-1])), na.rm = TRUE)
			})
			smallest <- which(dists == min(dists))
			if(length(smallest) > 1) { smallest <- smallest[1] }
			comp_order <- perms[[smallest]]

			oob_cluster_mapping[[i]] <- comp_order

			rows <- list()
			for(j in 1:length(clusters)) {
				rows[[j]] <- which(oob$iter == i & oob$cluster == comp_order[j])
			}
			for(j in 1:length(clusters)) {
				if(length(rows[[j]] > 0)) {
					oob[rows[[j]],]$cluster <- clusters[j]
				}
			}
		}
	}

	cv$in_sample <- is
	cv$oob_sample <- oob
	cv$in_cluster_mapping <- in_cluster_mapping
	cv$oob_cluster_mapping <- oob_cluster_mapping
	return(cv)
}
