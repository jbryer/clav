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
		greedy = (length(unique(cv$oob_sample$cluster)) > 7),
		var,
		...
) {
	oob <- cv$oob_sample
	is <- cv$in_sample
	base <- cv$complete_sample
	base_means <- reshape2::dcast(base, cluster ~ variable, value.var = 'mean')
	row.names(base_means) <- base_means$cluster
	clusters <- unique(oob$cluster) |> as.character()
	n_inters <- max(oob$iter)

	if(greedy) {
		if(missing(var)) {
			diffs <- sapply(
				unique(cv$complete_sample$variable),
				FUN = function(x) {
					cv$complete_sample[cv$complete_sample$variable == x,]$mean |> sort() |> diff() |> median()
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
		perms <- combinat::permn(clusters)

		if(length(clusters) > 7 & interactive()) {
			ans <- utils::menu(choices = c('Yes', 'No'),
							   title = paste0('With ', length(clusters), ' clusters there will be ',
							   			   length(perms), ' permutations to calculate. This may take a while.\n',
							   			   'Do you wish to continue with trying to align the cluster labels?'))
			if(ans == 2) {
				return(cv)
			}
		}

		# Fix in sample
		for(i in 1:n_inters) { # TODO: This is something that could be run in parallel to speed up
			comp <- is[is$iter == i,]
			comp_means <- reshape2::dcast(comp, cluster ~ variable, value.var = 'mean')
			row.names(comp_means) <- comp_means$cluster
			dists <- sapply(perms, FUN = function(x) {
				mean(as.matrix(abs(base_means[,-1] - comp_means[x,-1])))
			})
			comp_order <- perms[[which(dists == min(dists))]]

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
				mean(as.matrix(abs(base_means[,-1] - comp_means[x,-1])))
			})
			comp_order <- perms[[which(dists == min(dists))]]

			rows <- list()
			for(j in 1:length(clusters)) {
				rows[[j]] <- which(oob$iter == i & oob$cluster == comp_order[j])
			}
			for(j in 1:length(clusters)) {
				oob[rows[[j]],]$cluster <- clusters[j]
			}
		}
	}

	cv$in_sample <- is
	cv$oob_sample <- oob
	return(cv)
}
