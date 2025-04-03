load("data-raw/DAACS-WGU-SRL.rda")

library(clav)


cluster_vars <- c('Motivation', 'Metacogntion', 'Strategies', 'Mathematics', 'Reading', 'Writing')
names(daacs.wgu)[1:6] <- cluster_vars

daacs.wgu <- daacs.wgu[complete.cases(daacs.wgu[,cluster_vars]),]
daacs.wgu$LogFeedbackViews <- log(daacs.wgu$FeedbackViews)

optimal <- optimal_clusters(daacs.wgu[,cluster_vars], max_k = 6)
optimal
plot(optimal, ncol = 2)

cv <- cluster_validation(daacs.wgu[,cluster_vars],
						 n_clusters = 5)
plot(cv, facet = FALSE)

cv_boot <- cluster_validation(daacs.wgu[,cluster_vars],
						 n_clusters = 5,
						 sample_size = nrow(daacs.wgu),
						 replace = TRUE)
plot(cv_boot, facet = FALSE)

daacs.wgu[,cluster_vars] <- daacs.wgu[,cluster_vars] |> dplyr::mutate_if(is.numeric, scale_this)

fit <- stats::kmeans(daacs.wgu[,cluster_vars], centers = 5)
profile_plot(daacs.wgu[,cluster_vars],
			 clusters = fit$cluster,
			 df_dep = daacs.wgu[,c('LogFeedbackViews', 'OnTime_Term1')],
			 cluster_order = cluster_vars)
