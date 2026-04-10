library(clav)
library(dplyr)
library(ggplot2)

daacs_cache_file <- 'data/daacs.Rda'
daacs_cv_cache_file <- 'data/daacs_cv.Rda'

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

	save(daacs, daacs_oc, daacs_af, file = daacs_cache_file)
} else {
	load(daacs_cache_file)
}

if(!file.exists(daacs_cv_cache_file)) {
	daacs_cv <- list()
	for(i in 2:6) {
		daacs_cv[[paste0('k', i)]] <- clav::cluster_validation(daacs[,daacs_cluster_vars], n_clusters = i)
	}

	save(daacs_cv, file = daacs_cv_cache_file)
} else {
	load(daacs_cv_cache_file)
}

