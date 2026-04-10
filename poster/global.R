# Packages for the shiny poster
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(shinyjs)
library(shinydashboard)
library(gt)
library(gtsummary)
library(shinyfullscreen)
library(dplyr)

# Add other packages here
library(GGally)
library(clav)

source('R/utilities.R')
source('R/renderRmd.R')

ggplot2::theme_set(ggplot2::theme_minimal())

# Set the accuracy for ggplot2 labels
pct_format = scales::percent_format(accuracy = .1)

poster_link <- 'https://github.com/jbryer/clav'

# Generate QR code
png('www/qrcode.png')
qrcode::qr_code(poster_link) |> plot()
dev.off()

navbar_title <- div(a(img(src="special_k_logo.png", height=35), # image must be in www/ folder
					  href= poster_link, target='_new'),
					style = "position: relative; top: -5px;")

poster_title <- 'Bootstrapping to Determine the Optimal Number of Clusters'
poster_subtitle <- 'April 10, 2026, National Council on Measurement in Education, Los Angeles, CA'

poster_authors <- div(
	style = 'font-size: 16px;',
	'Jason Bryer, Ph.D.',
	superscript(a(icon('envelope'), href='mailto:jason.bryer@cuny.edu', target='_new')), ' ',
	superscript(a(icon('mastodon'), href='https://vis.social/@jbryer', target='_new')), ' ',
	superscript(a(icon('github'), href='https://github.com/jbryer', target='_new')), ' ',
	superscript(a(icon('globe'), href='https://bryer.org', target='_new'))
)

background_color <- "ghostwhite"
box_background_color <- 'ghostwhite'

# These tabs will have a white background
tabs_with_white_background <- c('Paper')

# This will force running the app in the default web browser.
if(interactive()) {
   options(shiny.launch.browser = .rs.invokeShinyWindowExternal)
}

##### Load data  ###############################################################
cache_dir <- 'cache'
if(!dir.exists(cache_dir)) {
	dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
}

data("penguins", package = "palmerpenguins")
penguins <- penguins |>
	dplyr::select(species, bill_length_mm, flipper_length_mm) |>
	tidyr::drop_na() |>
	dplyr::mutate(dplyr::across(
		dplyr::all_of(c('bill_length_mm', 'flipper_length_mm')),
		clav::scale_this))

data("iris", package = "datasets")
iris <- iris |>
	select(Species, Sepal.Length, Petal.Length) |>
	tidyr::drop_na() |>
	dplyr::mutate(dplyr::across(
		dplyr::all_of(c('Sepal.Length', 'Petal.Length')),
		clav::scale_this))

data('daacs', package = 'clav')

data_vars <- list(
	penguins = list(
		cluster_vars = c('bill_length_mm', 'flipper_length_mm'),
		cluster_var = 'species',
		data = penguins
	),
	iris = list(
		cluster_vars = c('Sepal.Length', 'Petal.Length'),
		cluster_var = 'Species',
		data = iris
	)
)


cluster_overlap_fits <- list()
cluster_agreement_fits <- list()
optimal_clusters_fits <- list()

for(i in seq_len(length(data_vars))) {
	data_name <- names(data_vars)[i]
	# Optimal clusters
	oc_cache_file <- file.path(cache_dir, paste0(data_name, '_oc.rds'))
	if(file.exists(oc_cache_file)) {
		optimal_clusters_fits[[data_name]] <- readRDS(oc_cache_file)
	} else{
		oc <- clav::optimal_clusters(
			data_vars[[i]]$data[,data_vars[[i]]$cluster_vars]
		)
		saveRDS(oc, oc_cache_file)
		optimal_clusters_fits[[data_name]] <- oc
	}
	# Cluster overlap fit
	cof_cache_file <- file.path(cache_dir, paste0(data_name, '_cof.rds'))
	if(file.exists(cof_cache_file)) {
		cluster_overlap_fits[[data_name]] <- readRDS(cof_cache_file)
	} else {
		cof <- clav::cluster_overlap_fit(
			data_vars[[i]]$data[,data_vars[[i]]$cluster_vars]
		)
		saveRDS(cof, file = cof_cache_file)
		cluster_overlap_fits[[data_name]] <- cof
	}
	# Cluster agreement fit
	caf_cache_file <- file.path(cache_dir, paste0(data_name, '_caf.rds'))
	if(file.exists(caf_cache_file)) {
		cluster_agreement_fits[[data_name]] <- readRDS(caf_cache_file)
	} else {
		caf <- clav::cluster_agreement_fit(
			data_vars[[i]]$data[,data_vars[[i]]$cluster_vars]
		)
		saveRDS(caf, file = caf_cache_file)
		cluster_agreement_fits[[data_name]] <- caf
	}
}


##### Setup data for demographics tab ##########################################
# poster_data <- penguins
# primary_variables <- names(poster_data)
# secondary_variables <- names(poster_data)

if(FALSE) {
	input <- list(dataset = 'penguins')

	thedata <- data_vars[[input$dataset]]$data
	cluster_vars <- data_vars[[input$dataset]]$cluster_vars
	cluster_var <- data_vars[[input$dataset]]$cluster_var
	ggplot(thedata, aes(x = .data[[cluster_vars[1]]], y = .data[[cluster_vars[2]]],
						color = .data[[cluster_var]])) +
		geom_point()

	ca <- cluster_agreement_fits[[input$dataset]]
	ls(ca)
	class(ca$k3$cv)
	plot(ca$k3$cv)
	plot_distributions(ca$k3$cv)

	cv <- cluster_validation(thedata[,cluster_vars], n_clusters = 3)
	plot(cv)
	plot_distributions(cv)

	fit <- stats::kmeans(thedata[,cluster_vars], 3)
	thedata$fitted_cluster <- LETTERS[fit$cluster]
	ggplot(thedata, aes(x = .data[[cluster_vars[1]]], y = .data[[cluster_vars[2]]],
						color = .data[['fitted_cluster']])) +
		geom_point()

}
