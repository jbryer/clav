library(clav)
library(dplyr)
library(ggplot2)

daacs_cache_file <- 'cache/daacs.Rda'

if(!file.exists(daacs_cache_file)) {
	data("daacs", package = 'clav')

	daacs_cluster_vars <- c('Motivation', 'Metacognition', 'Strategies', 'Mathematics', 'Reading', 'Writing')
	daacs_outcome_vars <- c('FeedbackViews', 'TermSuccess')

	# Standardize the scores
	daacs <- daacs |>
		dplyr::mutate(dplyr::across(dplyr::all_of(daacs_cluster_vars), clav::scale_this))

	daacs_oc <- clav::optimal_clusters(daacs[,daacs_cluster_vars])
	plot(daacs_oc) |> print()

	daacs_of <- clav::cluster_overlap_fit(daacs[,daacs_cluster_vars])
	plot(daacs_of)

	daacs_af <- clav::cluster_agreement_fit(daacs[,daacs_cluster_vars])
	plot(daacs_af)

	daacs_cv <- list()
	for(i in 2:6) {
		daacs_cv[[paste0('k', i)]] <- clav::cluster_validation(daacs[,daacs_cluster_vars], n_clusters = i)
	}

	save(daacs, daacs_oc, daacs_af, daacs_cv, file = daacs_cache_file)
} else {
	load(daacs_cache_file)
}

# daacs_fit <- stats::kmeans(daacs[,cluster_vars], 5)
# clav::profile_plot(df = daacs[,daacs_cluster_vars],
# 				   clusters = LETTERS[daacs_fit$cluster],
# 				   df_dep = daacs[,daacs_outcome_vars,drop=FALSE])

