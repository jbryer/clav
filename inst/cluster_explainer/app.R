library(shiny)
library(VisualStats)
library(ggplot2)
library(clav)

ggplot2::theme_set(ggplot2::theme_minimal())

palette <- c('#1f78b4','#b2df8a','#fb9a99','#e31a1c','#fdbf6f','#33a02c',
			 '#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928','#a6cee3')

data("penguins")
penguins <- penguins |> na.omit()
row.names(penguins) <- 1:nrow(penguins)
data("iris")
iris <- iris |> na.omit()
row.names(iris) <- 1:nrow(iris)

dataframes <- list(
	'penguins' = list(
		dataframe = penguins,
		cluster_vars = c('bill_len', 'flipper_len'),
		cluster_var = 'species'
	),
	'iris' = list(
		dataframe = iris,
		cluster_vars = c('Sepal.Length', 'Sepal.Width'),
		cluster_var = 'Species'
	)
)

##### UI  ##########################################################################################
ui <- fluidPage(
    titlePanel("Cluster Analysis Explainer"),

    sidebarLayout(
        sidebarPanel(
        	selectInput(
        		inputId = 'dataset',
        		label = "Dataset:",
        		choices = names(dataframes),
        		selected = "penguins"
        	),
        	conditionalPanel(
        		'input.method != "hclust"',
        		sliderInput(
        			inputId = "k",
        			label = "Number of clusters (k):",
        			min = 2,
        			max = 10,
        			value = 3)
        	),
        	selectInput(
        		inputId = 'method',
        		label = 'Clustering Method:',
        		choices = c('K-means' = 'kmeans',
        					'Hierarchical Clustering' = 'hclust'),
        		selected = 'kmeans'
        	),
        	uiOutput('iteration_input'),
        	actionButton(inputId = 'refresh', label = 'Refresh', icon = icon('rotate'))
        ),

        # Show a plot of the generated distribution
        mainPanel(
        	tabsetPanel(
        		tabPanel(
        			'Estimation',
        			plotOutput('interation_plot', height = '600px')
        		),
        		tabPanel(
        			'Raw Plot',
        			plotOutput('scatter_plot', height = '600px')
        		),
        		tabPanel(
        			'Data Table',
        			DT::DTOutput('datatable')
        		)
        	)
        )
    )
)

##### Server #######################################################################################
server <- function(input, output) {
	get_data_raw <- reactive({
		dataframes[[input$dataset]]$dataframe
	})

	get_data <- reactive({
		get_data_raw() |>
			dplyr::select(dplyr::all_of(dataframes[[input$dataset]]$cluster_vars)) |>
			na.omit() |>
			dplyr::mutate(dplyr::across(where(is.numeric), clav::scale_this))
	})

	get_cluster_vars <- reactive({
		dataframes[[input$dataset]]$cluster_vars
	})

	get_cluster_var <- reactive({
		dataframes[[input$dataset]]$cluster_var
	})

	output$datatable <- DT::renderDT({
		get_data_raw()
	})

	get_kmeans <- reactive({
		input$refresh
		vars <- get_data()
		clav::kmeans_iterative(vars, input$k)
	})

	get_hclust <- reactive({
		input$refresh
		vars <- get_data()
		stats::hclust(stats::dist(vars))
	})

	output$iteration_input <- renderUI({
		min <- 0
		max <- 0
		if(input$method == 'kmeans') {
			min <- 1
			max <- length(get_kmeans())
		} else if(input$method == 'hclust') {
			min <- nrow(get_data()) - 1
			max <- 1
		} else {
			stop('Unknow clustering method')
		}
		if(require(shinyWidgets)) {
			shinyWidgets::sliderTextInput(
				inputId = 'iteration',
				label = 'Iteration',
				choices = seq(min, max, ifelse(min > max, -1, 1)),
				animate = TRUE,
			)
		} else {
			warning('shinyWidgets package not found. Slider animation may not be accurate.')
			sliderInput(
				inputId = 'iteration',
				label = 'Iteration',
				min = min(c(min, max)),
				max = max(c(min, max)),
				value = min,
				animate = TRUE,
				step = 1
			)
		}
	})

	output$interation_plot <- renderPlot({
		req(input$iteration)
		req(input$k)
		p <- NULL
		df <- get_data()

		if(input$method == 'kmeans') {
			results <- get_kmeans()
			df$cluster <- factor(results[[input$iteration]]$clusters)
			centers <- results[[input$iteration]]$centers
			centers$cluster <- factor(1:nrow(centers))
			p <- ggplot(df, aes(x = .data[[names(df)[1]]], y = .data[[names(df)[2]]], color = cluster)) +
				geom_point() +
				geom_point(data = centers, size = 5) +
				geom_point(data = centers, shape = 1, color = 'black', size = 5, stroke = 2) +
				theme(legend.position = 'bottom') +
				ggtitle(paste0('Iteration: ', input$iteration))
		} else if(input$method == 'hclust') {
			hc <- get_hclust()
			cluster_assign <- cutree(hc, k = input$iteration)
			tab <- table(cluster_assign)
			assigned <- names(tab[tab > 1])
			rows <- as.integer(names(cluster_assign[cluster_assign %in% assigned]))
			assigned <- df[rows,]
			assigned$cluster <- cluster_assign[rows]
			centers <- clav::get_centers(assigned[,1:2], assigned$cluster) |> na.omit()
			centers$cluster <- row.names(centers)
			p <- ggplot(df[-rows,],
						 aes(x = .data[[names(df)[1]]], y = .data[[names(df)[2]]])) +
				geom_point(alpha = 0.5, size = 1.5) +
				geom_point(data = assigned, aes(color = factor(cluster)), alpha = 0.75) +
				geom_point(data = centers, size = 5, aes(color = factor(cluster))) +
				geom_point(data = centers, shape = 1, color = 'black', size = 5, stroke = 2) +
				theme(legend.position = 'none') +
				ggtitle(paste0('k = ', input$iteration))
		}

		return(p)
	})

	output$scatter_plot <- renderPlot({
		df <- get_data_raw()
		cluster_vars <- get_cluster_vars()
		cluster_var <- get_cluster_var()
		ggplot(df, aes(x = .data[[cluster_vars[1]]], y = .data[[cluster_vars[2]]], color = .data[[cluster_var]])) +
			geom_point()
	})
}

##### Run ##########################################################################################
shinyApp(ui = ui, server = server)
