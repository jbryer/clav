utils::globalVariables(c("data_frames"))

#' Run the Shiny application.
#'
#' @param ... named list of data frames to make available in the Shiny application.
#' @rdname clav_shiny
#' @export
cluster_shiny <- function(...) {
	data_frames <- list(...)
	if(length(data_frames) == 1 & names(data_frames) == '') {
		names(data_frames)[1] <- 'Data'
	}
	if(is.null(names(data_frames))) {
		stop('Data frames must be named.')
	} else if(any(names(data_frames) == '')) {
		stop('All data frames listed must be named.')
	}

	server <- clav_shiny_server
	ui <- clav_shiny_ui

	app_env <- new.env()
	assign('data_frames', data_frames, app_env)

	environment(server) <- as.environment(app_env)
	environment(ui) <- as.environment(app_env)

	shiny::shinyApp(ui = ui, server = server)
}

#' UI for cluster analysis application
#' @rdname clav_shiny
#' @export
clav_shiny_ui <- function() {
		shiny::fluidPage(
		shiny::titlePanel('Cluster Analysis'),
		shiny::sidebarLayout(
			shiny::sidebarPanel(
				width = 3,
				shiny::uiOutput('data_frame_select'),
				n_clusters_input('shinyclav'),
				cluster_variable_input('shinyclav'),
				dependent_variable_input('shinyclav'),
				shiny::selectInput('clustering_algo',
								   label = 'Clustering Algorithm',
								   choices = c('kmeans'),
								   selected = 'kmeans')
			),
			shiny::mainPanel(
				width = 9,
				n_cluster_message('shinyclav'),
				shiny::tabsetPanel(
					shiny::tabPanel(
						title = 'Clusters',
						n_cluster_plot_output('shinyclav'),
						shiny::hr(),
						cluster_size_bar_plot_output('shinyclav', height = '300px')
					),
					shiny::tabPanel(
						title = 'Variable Plots',
						profile_plot_output('shinyclav', height = '600px')
					),
					shiny::tabPanel(
						title = 'Pairs Plot',
						cluster_pairs_plot_output('shinyclav', height = '600px')
					),
					shiny::tabPanel(
						title = 'Cluster Plots',
						bivariate_cluster_plot_output('shinyclav'),
						discriminant_projection_plot_output('shinyclav')
					),
					shiny::tabPanel(
						title = 'Dependent Variable',
						dependent_variable_plot_output('shinyclav'),
						dependent_variable_table_output('shinyclav'),
						dependent_null_hypothesis_output('shinyclav')
					)
				)
			)
		)
	)
}

#' Server function for cluster analysis application
#' @rdname clav_shiny
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @export
clav_shiny_server <- function(input, output, session) {
	get_data <- reactive({
		req(input$data_frame_select)
		return(data_frames[[input$data_frame_select]])
	})

	output$data_frame_select <- shiny::renderUI({
		shiny::selectInput(inputId = 'data_frame_select',
						   label = 'Select data:',
						   choices = names(data_frames))
	})

	cluster_module('shinyclav', data = get_data)
}
