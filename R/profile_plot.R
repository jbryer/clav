#' Profile plot for cluster analysis.
#'
#' @param df data.frame with the columns used for the cluster analysis.
#' @param clusters vector indicating what cluster each row of `df` belongs to.
#' @param df_dep a data.frame with any dependent variables to include in the plot (optional).
#' @param standardize if TRUE values in `df` will be converted to z-scores.
#' @param bonferroni if TRUE Bonferroni adjusted error bars will be plotted.
#' @param label_means label the mean values of both clusters and outcome variables.
#' @param label_profile_means label the mean values of the clusters.
#' @param label_outcome_means label the mean values of outcome variables.
#' @param center_band the percentage around the mean to shade. This only works
#'        if `standardize = TRUE`.
#' @param center_fill the color of the center band.
#' @param center_alpha the transparency level of the center band.
#' @param text_size text size.
#' @param hjust horizontal adjustment of labels.
#' @param point_size size of points passed to [ggplot2::geom_point()].
#' @param se_factor critical value used ot determine the width of standard error bars.
#' @param color_palette the color palette to use. See `ggplot2:scale_color_brewer()`
#'        for more details.
#' @param cluster_labels labels for the clusters.
#' @param cluster_order order of clusters on the x-axis.
#' @param label_clusters whether to label clusters on the main panel.
#' @param cluster_label_x cluster labels.
#' @param cluster_label_hjust horizontal adjustment for y-axis labels.
#' @param ylab label for the y-axis.
#' @param title plot title.
#' @import ggplot2
#' @importFrom dplyr mutate_if all_of
#' @importFrom reshape2 melt
#' @importFrom psych describeBy
#' @importFrom latex2exp TeX
#' @importFrom stats aov chisq.test dist pnorm predict qnorm sd var
#' @export
profile_plot = function(
		df,
		clusters,
		df_dep,
		standardize = TRUE,
		bonferroni = TRUE,
		label_means = TRUE,
		label_profile_means = label_means,
		label_outcome_means = label_means,
		center_band = 0.25,
		center_fill = '#f0f9e8',
		center_alpha = 0.1,
		text_size = 4,
		hjust = 0.5,
		point_size = 2,
		se_factor = 1.96,
		color_palette = 2,
		cluster_labels,
		cluster_order,
		label_clusters = TRUE,
		cluster_label_x,
		cluster_label_hjust = 5,
		ylab = ifelse(standardize, 'Mean Standard Score', 'Mean Score'),
		title = 'Cluster Profiles'
) {
	if(length(clusters) != nrow(df)) {
		stop('length of clusters is not the same as the number of rows in df.')
	}

	if(!is.factor(clusters)) {
		clusters <- as.factor(clusters)
	}

	n_clusters <- clusters |> levels() |> length()

	if(missing(cluster_labels)) {
		cluster_labels <- levels(clusters)
	} else {
		cluster_labels <- paste0(LETTERS[1:n_clusters], ': ', cluster_labels)
	}

	print_p_value <- function(p_value) {
		if(p_value < 0.01) {
			"p < 0.01"
		} else {
			paste0("p = ", round(p_value, digits = 2))
		}
	}

	scale_this <- function(x) {
		(x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
	}

	if(standardize) {
		df <- df |> dplyr::mutate_if(is.numeric, scale_this)
	}

	plots <- list()

	nClusters <- clusters |> unique() |> length()
	df.melted <- reshape2::melt(cbind(df, cluster = as.character(clusters)), id.vars = 'cluster')
	tab <- psych::describeBy(df.melted$value,
							 group = list(df.melted$cluster, df.melted$variable),
							 mat = TRUE)
	tab <- tab[,c('group1', 'group2', 'n', 'mean', 'se', 'sd', 'median')]
	names(tab)[1:2] <- c('Cluster', 'Factor')

	if(!missing(cluster_order)) {
		tab$Factor <- factor(tab$Factor,
							 levels = cluster_order,
							 ordered = TRUE)
	}

	if(is.null(color_palette)) {
		plots[[length(plots)+1]] <- ggplot(
			tab[order(tab$Factor),],
			aes(x = .data$Factor, y = .data$mean, group = .data$Cluster))
	} else {
		plots[[length(plots)+1]] <- ggplot(
			tab[order(tab$Factor),],
			aes(x = .data$Factor, y = .data$mean, color = .data$Cluster, group = .data$Cluster))

	}

	if(!is.null(center_band) & !is.na(center_band)) {
		plots[[length(plots)]] <- plots[[length(plots)]] +
			geom_rect(xmin = -Inf, xmax = Inf,
					  ymin = -qnorm(0.5 - center_band/2), ymax = qnorm(0.5 - center_band/2),
					  alpha = center_alpha, fill = center_fill, color = NA) +
			geom_hline(yintercept = c(-qnorm(0.5 - center_band/2), qnorm(0.5 - center_band/2)),
					   color = 'grey70', linetype = 2)
	}

	plots[[length(plots)]] <- plots[[length(plots)]] +
		geom_hline(yintercept = 0, color = 'grey70') +
		geom_path() +
		geom_point(size = point_size) +
		geom_errorbar(aes(ymin = .data$mean - se_factor * .data$se,
						  ymax = .data$mean + se_factor * .data$se),
					  width = 0.25, alpha = 0.5) +
		xlab('') + ylab(ylab) +
		scale_color_brewer(type = 'qual', palette = color_palette, labels = cluster_labels) +
		theme_minimal() +
		theme(legend.position = 'bottom',
			  legend.key.size = unit(0, 'lines'),
			  legend.text = element_text(size = 10)) +
		ggtitle(title, subtitle = paste0('k = ', nClusters))

	if(label_clusters) {
		if(missing(cluster_label_x)) {
			cluster_label_x <- levels(tab$Factor)[1]
		}

		plots[[length(plots)]] <- plots[[length(plots)]] +
			geom_text(data = tab[tab$Factor == cluster_label_x,],
					  aes(label = .data$Cluster),
					  hjust = cluster_label_hjust)
	}

	if(label_profile_means) {
		plots[[length(plots)]] <- plots[[length(plots)]] +
			geom_label(aes(label = round(.data$mean, digits = 2)),
					   fill = 'white', hjust = hjust, size = text_size, show.legend = FALSE)
	}

	if(missing(df_dep)) {
		return(plots[[1]])
	} else {
		if(is.vector(df_dep)) {
			df_dep <- data.frame(y = df_dep)
		}

		for(i in 1:ncol(df_dep)) {
			dep_tab <- psych::describeBy(df_dep[,i,drop=TRUE],
										 group = list(clusters), mat = TRUE)
			dep_tab$group1 <- factor(dep_tab$group1,
									 levels = levels(clusters),
									 labels = cluster_labels)

			if(is.null(color_palette)) {
				plots[[length(plots)+1]] <- ggplot(
					dep_tab,
					aes(x = .data$group1, y = .data$mean))
			} else {
				plots[[length(plots)+1]] <- ggplot(
					dep_tab,
					aes(x = .data$group1, y = .data$mean, color = .data$group1))
			}
			plots[[length(plots)]] <- plots[[length(plots)]] +
				geom_errorbar(aes(ymin = .data$mean - se_factor * .data$se,
								  ymax = .data$mean + se_factor * .data$se,
								  color = .data$group1), width = 0.5) +
				geom_point(size = point_size) +
				xlab('Cluster') + ylab('') +
				theme_minimal() +
				scale_color_brewer(type = 'qual', palette = color_palette) +
				scale_x_discrete('', labels = levels(clusters)) +
				theme(legend.position = 'none')

			if(bonferroni) {
				n_pairs <- choose(length(unique(clusters)), 2)
				p_threshold <- pnorm(-1 * se_factor) * 2
				bonferroni_factor <- qnorm((p_threshold / n_pairs) / 2)
				plots[[length(plots)]] <- plots[[length(plots)]] +
					geom_errorbar(aes(ymin = .data$mean - bonferroni_factor * .data$se,
									  ymax = .data$mean + bonferroni_factor * .data$se,
									  color = .data$group1),
								  width = 0.5, linetype = 2)
			}

			if(label_outcome_means) {
				plots[[length(plots)]] <- plots[[length(plots)]] +
					geom_label(aes(label = paste0(round(.data$mean, digits = 2))),
							   fill = 'white', hjust = hjust, size = text_size)
			}


			col <- df_dep[,i,drop=TRUE]
			if(is.factor(col) | is.character(col) | is.logical(col)) { # chi-squared
				chisq_out <- chisq.test(clusters, col)
				x2_str <- paste0("$\\chi^2$ = ", round(chisq_out$statistic, digits = 2), ", ",
								 print_p_value(chisq_out$p.value))
				plots[[length(plots)]] <- plots[[length(plots)]] +
					ggtitle(names(df_dep)[i], subtitle = latex2exp::TeX(x2_str))
			} else { # ANOVA
				aov_out <- aov(col ~ clusters) |> summary()
				f_str <- paste0("$F_{", paste0(aov_out[[1]][1:2,1], collapse = ", "), "}$ = ",
								round(aov_out[[1]][1,4], digits = 2), ", ",
								print_p_value(aov_out[[1]][1,5]))
				plots[[length(plots)]] <- plots[[length(plots)]] +
					ggtitle(names(df_dep)[i], subtitle = latex2exp::TeX(f_str))
			}
		}

		grobs <- ggplotGrob(plots[[1]])$grobs
		legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

		plots[[1]] <- plots[[1]] + theme(legend.position = 'none')
		plots$nrow <- 1
		plots$rel_widths <- c(3, rep(1, ncol(df_dep)))

		cowplot::plot_grid(
			do.call(
				cowplot::plot_grid,
				args = plots
			),
			legend,
			ncol = 1,
			rel_heights = c(10, 1))
	}
}
