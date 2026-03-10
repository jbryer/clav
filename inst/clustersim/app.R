library(shiny)
library(clav)
library(ggplot2)

ggplot2::set_theme(ggplot2::theme_minimal())

ui <- fluidPage(
    titlePanel("Cluster Analysis Simulation"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("k",
                        "Number of clusters:",
                        min = 2,
                        max = 6,
                        value = 3),
            numericInput('n',
            			 'n per cluster',
            			 min = 5, max = 100, value = 30),
            uiOutput('ui_inputs')
        ),

        mainPanel(
           plotOutput("cluster_plot")
        )
    )
)

server <- function(input, output) {
	output$ui_inputs <- renderUI({
		ui <- list()
		for(i in seq_len(input$k)) {
			ui[[length(ui) + 1]] <- strong(paste0('Cluster ', LETTERS[i]))

			ui[[length(ui) + 1]] <- fluidRow(
				column(
					6,
					numericInput(
						inputId = paste0('mean_x_', i),
						label = paste0('Mean x'),
						value = (i - 1) * 3)
				),
				column(
					6,
					numericInput(
						inputId = paste0('sd_x_', i),
						label = paste0('SD x'),
						value = 1,
						min = 1)
				)
			)
			ui[[length(ui) + 1]] <- fluidRow(
				column(
					6,
					numericInput(
						inputId = paste0('mean_y_', i),
						label = paste0('Mean y'),
						value = (i - 1) * 3)
				),
				column(
					6,
					numericInput(
						inputId = paste0('sd_y_', i),
						label = paste0('SD y'),
						value = 1)
				)
			)
			ui[[length(ui) + 1]] <- hr()
		}
		return(ui)
	})

	get_data <- reactive({
		dfs <- list()
		for(i in seq_len(input$k)) {
			dfs[[i]] <- data.frame(
				cluster = rep(LETTERS[i], input$n),
				x = rnorm(n = input$n,
						  mean = input[[paste0('mean_x_', i)]],
						  sd = input[[paste0('sd_x_', i)]]),
				y = rnorm(n = input$n,
						  mean = input[[paste0('mean_y_', i)]],
						  sd = input[[paste0('sd_y_', i)]])
			)
		}
		return(do.call(rbind, dfs))
	})

    output$cluster_plot <- renderPlot({
    	df <- get_data()
    	ggplot(df, aes(x = x, y = y, color = cluster)) +
    		geom_point()
    })
}

# Run the application
shinyApp(ui = ui, server = server)
