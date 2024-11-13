#'
#'
#' @export
n_cluster_message <- function(id) {
	shiny::textOutput(NS(id, id = 'n_message'))
}

#'
#'
#' @export
n_clusters_input <- function(id, label = "Number of clusters:",
						   min = 2, max = 10, value = 4) {
	tagList(
		shiny::sliderInput(inputId = NS(id, id = "k"),
						   label = label,
						   min = 2,
						   max = 10,
						   value = 4)
	)
}

#'
#'
#' @export
cluster_variable_select_intput <- function(id) {
	shiny::uiOutput(NS(id, id = 'variable_selection'))
}

#'
#' @export
n_cluster_plot <- function(id) {
	shiny::plotOutput(NS(id, id = 'n_clusters_plot'))
}

#'
#'
#' @param ... other parameters passed to [shiny::plotOutput()]
#' @export
cluster_size_bar_plot <- function(id, ...) {
	shiny::plotOutput(NS(id, id = 'cluster_size_bar'), ...)
}

#'
#'
#' @param ... other parameters passed to [shiny::plotOutput()]
#' @export
variable_cluster_plot <- function(id, ...) {
	shiny::plotOutput(NS(id, id ='variable_cluster_plot'), height = '600px')
}

#'
#'
#' @param ... other parameters passed to [shiny::plotOutput()]
#' @export
cluster_pairs_plot <- function(id, ...) {
	shiny::plotOutput(NS(id, id = 'cluster_pairs_plot'), height = '600px')
}

#'
#'
#' @param ... other parameters passed to [shiny::plotOutput()]
#' @export
bivariate_cluster_plot <- function(id, ...) {
	shiny::plotOutput(NS(id, id = 'bivariate_cluster_plot'), ...)
}

#'
#'
#' @param ... other parameters passed to [shiny::plotOutput()]
#' @export
discriminant_projection_plot <- function(id, ...) {
	shiny::plotOutput(NS(id, id ='discriminant_projection_plot'), ...)
}

#' Convert to z-score.
#'
#' @param x numeric vector to convert to standard (z) score.
scale_this <- function(x) {
	(x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

#'
#' @export
dependent_variable_select <- function(id) {
	shiny::uiOutput(NS(id, id = 'dependent_variable_ui'))
}

#'
#' @export
dependent_variable_plot <- function(id, ...) {
	shiny::plotOutput(NS(id, id = 'dependent_plot'), ...)
}

#'
#' @export
dependent_variable_table <- function(id) {
	shiny::tableOutput(NS(id, id = 'dependent_table'))
}

#'
#' @export
dependent_null_hypothesis <- function(id) {
	shiny::verbatimTextOutput(NS(id, id = 'dependent_null_hypothesis'))
}

#' Shiny module for cluster analysis.
#'
#' @param id ID for the module.
#' @param data a function to return the data (probably a reactive function).
#' @export
cluster_module <- function(id,
						   data,
						   default_vars = c(),
						   default_dependent_variable = NULL,
						   ci = 1) {
	moduleServer(id, function(input, output, session) {
		get_data_raw <- shiny::reactive({
			if(is.reactive(data)) {
				data()
			} else {
				data
			}
		})

		get_data <- shiny::reactive({
			# req(input$variable_selection)
			get_data_raw() |>
				dplyr::select(c(input$variable_selection, input$dependent_variable)) %>%
				dplyr::filter(complete.cases(.)) |>
				dplyr::mutate_if(is.numeric, scale_this)
		})

		get_cluster_fit <- shiny::reactive({
			req(input$k)
			req(input$variable_selection)

			thedata <- get_data()
			fit <- kmeans(thedata[,input$variable_selection], input$k)
			return(fit)
		})

		output$n_message <- shiny::renderText({
			thedata <- get_data()
			return(paste0(prettyNum(nrow(thedata), big.mark = ','),
						  ' observations used from ',
						  prettyNum(nrow(get_data_raw()), big.mark = ','),
						  ' total available (',
						  round(nrow(thedata) / nrow(get_data_raw()) * 100, 2), '%).'))
		})

		output$variable_selection <- shiny::renderUI({
			shiny::selectizeInput(
				inputId = NS(id, id ='variable_selection'),
				label = 'Variables to include',
				choices = names(get_data_raw()),
				multiple = TRUE,
				selected = default_vars
			)
		})

		output$dependent_variable_ui <- shiny::renderUI({
			shiny::selectInput(
				inputId = NS(id, id = 'dependent_variable'),
				label = 'Dependent variable',
				choices = names(get_data_raw()),
				multiple = FALSE,
				selected = default_dependent_variable
			)
		})

		output$regression_output <- renderPrint({
			req(input$dependent_variable)

		})

		output$n_clusters_plot <- shiny::renderPlot({
			req(input$variable_selection)
			thedata <- get_data()
			wss <- (nrow(thedata) - 1) * sum(apply(thedata, 2, var))
			for(i in 2:10) { # Calculate within sum of quares for up to 10 clusters
				wss[i] <- sum(kmeans(thedata, centers = i)$withinss)
			}
			plot(1:10, wss, type = 'b',
				 xlab = 'Number of Clusters',
				 ylab = 'Within group sum of squares')
		})

		output$cluster_size_bar <- shiny::renderPlot({
			fit <- get_cluster_fit()
			thedata <- get_data()
			thedata$cluster <- factor(fit$cluster, labels = letters[1:input$k])
			ggplot(thedata, aes(x = cluster,
							  label = scales::percent(prop.table(stat(count))))) +
				geom_bar() +
				geom_text(stat = 'count',
						  position = position_dodge(.9),
						  vjust = -0.5,
						  size = 4) +
				ggtitle('Cluster Sizes')
		})

		output$variable_cluster_plot <- shiny::renderPlot({
			fit <- get_cluster_fit()
			thedata <- get_data() |> dplyr::select(!input$dependent_variable)
			thedata$cluster <- factor(fit$cluster, labels = letters[1:input$k])

			thedata.melted <- melt(thedata, id.vars = 'cluster')
			tab <- describeBy(thedata.melted$value,
							  group = list(thedata.melted$cluster, thedata.melted$variable),
							  mat = TRUE)
			tab <- tab[,c('group1', 'group2', 'n', 'mean', 'se', 'sd', 'median')]
			names(tab)[1:2] <- c('Cluster', 'Factor')

			ggplot(tab[order(tab$Factor),],
				   aes(x = Factor, y = mean, color = Cluster, group = Cluster)) +
				geom_path(alpha = 0.5) +
				geom_point() +
				geom_errorbar(aes(ymin = mean - se_bar_multiplier * se, ymax = mean + se_bar_multiplier * se),
							  width = 0.25, alpha = 0.5) +
				geom_text(aes(label = round(mean, digits = 2)), hjust = -0.5, size = 4) +
				xlab('Scale') + ylab('Mean Standardized Scale Score')

		})

		output$cluster_pairs_plot <- shiny::renderPlot({
			fit <- get_cluster_fit()
			thedata <- get_data()
			thedata$cluster <- factor(fit$cluster, labels = letters[1:input$k])

			ggpairs(thedata,
					columns = names(thedata),
					axisLabels = 'show',
					mapping = ggplot2::aes_string(color='cluster'),
					# upper='blank',
					upper = list(continuous = "cor"),
					diag = list(continuous = "densityDiag", discrete = 'barDiag',
								aes_string(group = 'cluster', fill = 'cluster')),
					lower = list(continuous="density", combo="box", alpha = 0.2))

		})

		output$bivariate_cluster_plot <- shiny::renderPlot({
			fit <- get_cluster_fit()
			thedata <- get_data()
			thedata$cluster <- factor(fit$cluster, labels = letters[1:input$k])

			cluster::clusplot(thedata,
							  thedata$cluster,
							  color = TRUE,
							  shade = TRUE,
							  labels = 2,
							  lines = 0,
							  main = 'Bivariate Cluster Plot')
		})

		output$discriminant_projection_plot <- shiny::renderPlot({
			fit <- get_cluster_fit()
			thedata <- get_data()
			thedata$cluster <- factor(fit$cluster, labels = letters[1:input$k])

			fpc::plotcluster(thedata[,names(thedata) != 'cluster'],
							 thedata$cluster,
							 main = 'Discriminant projection plot')
		})

		output$dependent_plot <- shiny::renderPlot({
			thedata <- get_data()
			fit <- get_cluster_fit()
			thedata$cluster <- factor(fit$cluster, labels = letters[1:input$k])
			tab <- psych::describeBy(thedata[,input$dependent_variable,drop=TRUE],
									 group = thedata$cluster,
									 mat = TRUE)
			p <- ggplot(thedata, aes_string(x = 'cluster', y = input$dependent_variable)) +
				geom_boxplot() +
				geom_errorbar(data = tab, aes(x = group1, y = mean, ymin = mean - ci * se, ymax = mean + ci* se),
							  color = 'darkgreen', width = 0.5) +0
				geom_point(data = tab, aes(x = group1, y = mean), color = 'blue', size = 2)
			# ggsave(filename = 'test.png', plot = p)
			return(p)
		})

		output$dependent_table <- shiny::renderTable({
			thedata <- get_data()
			fit <- get_cluster_fit()
			thedata$cluster <- factor(fit$cluster, labels = letters[1:input$k])
			psych::describeBy(thedata[,input$dependent_variable,drop=TRUE],
							  group = thedata$cluster,
							  mat = TRUE) |>
				dplyr::select(group1, n, mean, sd, se, median, min, max) |>
				dplyr::rename(Cluster = group1)
		})

		output$dependent_null_hypothesis <- renderPrint({
			thedata <- get_data()
			dep <- thedata[,input$dependent_variable,drop=TRUE]
			fit <- get_cluster_fit()
			thedata$cluster <- factor(fit$cluster, labels = letters[1:input$k])
			if(is.numeric(dep) & length(unique(dep)) > 10) { # ANOVA
				formu <- as.formula(paste0(input$dependent_variable, ' ~ cluster'))
				aov(formu, data = thedata) |> summary()
			} else { # Chi-Squared
				chisq.test(thedata$cluster, dep)
			}
		})
	})
}
