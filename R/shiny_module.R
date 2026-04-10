utils::globalVariables(c("cluster", "count", "group1", "se", "n", "variable", "iter", "value"))

#' Output for printing status messages from the Shiny module.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @export
#' @rdname clav-shiny
n_cluster_message <- function(id) {
	shiny::textOutput(shiny::NS(id, id = 'n_message'))
}

#' Output to select which clustering method to use.
#'
#' @export
#' @rdname clav-shiny
cluster_method_input <- function(id) {
	shiny::selectInput(shiny::NS(id, id = 'clustering_algo'),
					   label = 'Clustering Algorithm',
					   choices = c('kmeans', 'hclust2'),
					   selected = 'kmeans')
}

#' Slider input for the desired number of clusters.
#'
#' @param label label for the slider input.
#' @param min The minimum value (inclusive) that can be selected.
#' @param max The maximum value (inclusive) that can be selected.
#' @param value The initial value of the slider.
#' @export
#' @rdname clav-shiny
n_clusters_input <- function(
		id,
		label = "Number of clusters:",
		min = 2,
		max = 10,
		value = 4
) {
	shiny::tagList(
		shiny::sliderInput(
			inputId = shiny::NS(id, id = "k"),
			label = label,
			min = 2,
			max = 10,
			value = 4)
	)
}

#' Input to select the variables to perform the cluster analysis with.
#'
#' @export
#' @rdname clav-shiny
cluster_variable_input <- function(id) {
	shiny::uiOutput(shiny::NS(id, id = 'variable_selection'))
}

#' Plot output for the figure to determine the optimal number of clusters.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @export
#' @rdname clav-shiny
n_cluster_plot_output <- function(id) {
	shiny::plotOutput(shiny::NS(id, id = 'n_clusters_plot'))
}

#' Plot output for the bar plot of cluster sizes.
#'
#' @param ... other parameters passed to [shiny::plotOutput()]
#' @export
#' @rdname clav-shiny
cluster_size_bar_plot_output <- function(id, ...) {
	shiny::plotOutput(shiny::NS(id, id = 'cluster_size_bar'), ...)
}

#' Plot output for the profiles.
#'
#' @seealso [profile_plot()]
#' @param ... other parameters passed to [shiny::plotOutput()]
#' @export
#' @rdname clav-shiny
profile_plot_output <- function(id, ...) {
	shiny::plotOutput(shiny::NS(id, id ='profile_plot'), height = '600px')
}

#' Plot output for the pairs plot.
#'
#' @seealso [GGally::ggpairs()]
#' @param ... other parameters passed to [shiny::plotOutput()]
#' @export
#' @rdname clav-shiny
cluster_pairs_plot_output <- function(id, ...) {
	shiny::plotOutput(shiny::NS(id, id = 'cluster_pairs_plot'), height = '600px')
}

#' Plot output for the bivariate cluster figure.
#'
#' @param ... other parameters passed to [shiny::plotOutput()]
#' @export
#' @rdname clav-shiny
bivariate_cluster_plot_output <- function(id, ...) {
	shiny::plotOutput(shiny::NS(id, id = 'bivariate_cluster_plot'), ...)
}

#' Plot output for the discriminant project figure.
#'
#' @param ... other parameters passed to [shiny::plotOutput()]
#' @export
#' @rdname clav-shiny
discriminant_projection_plot_output <- function(id, ...) {
	shiny::plotOutput(shiny::NS(id, id ='discriminant_projection_plot'), ...)
}

#' Shiny input to select the dependent (outcome) variable.
#'
#' @export
#' @rdname clav-shiny
dependent_variable_input <- function(id) {
	shiny::uiOutput(shiny::NS(id, id = 'dependent_variable_ui'))
}

#' Shiny output for the dependnet sample plot.
#'
#' @param ... other parmaeters passed to [shiny::plotOutput()].
#' @export
#' @rdname clav-shiny
dependent_variable_plot_output <- function(id, ...) {
	shiny::plotOutput(shiny::NS(id, id = 'dependent_plot'), ...)
}

#' Output of the dependent variable analysis.
#'
#' Table output for analyzing the dependent variable from the clusters. This will
#' either be an ANOVA for a quantitative dependent variable or a chi-squared test
#' for qualitative dependent variable.
#'
#' @export
#' @rdname clav-shiny
dependent_variable_table_output <- function(id) {
	shiny::tableOutput(shiny::NS(id, id = 'dependent_table'))
}

#' Output from the dependent varaible analysis.
#'
#' @export
#' @rdname clav-shiny
dependent_null_hypothesis_output <- function(id) {
	shiny::verbatimTextOutput(shiny::NS(id, id = 'dependent_null_hypothesis'))
}

#' Optimal clusters plot output.
#' @export
#' @rdname clav-shiny
optimal_clusters_plot_output <- function(id) {
	shiny::plotOutput(shiny::NS(id, id = 'optimal_clusters_plot'))
}

#' Cluster validation plot output.
#' @export
#' @rdname clav-shiny
cluster_valdiation_plot_output <- function(id) {
	shiny::plotOutput(shiny::NS(id, id = 'cluster_validation_plot'))
}

#' Cluster validation distributions plot.
#' @export
#' @rdname clav-shiny
cluster_validation_distribution_plot_output <- function(id) {
	shiny::plotOutput(shiny::NS(id, id = 'cluster_validation_distribution_plot'))
}

#' Shiny module for cluster analysis.
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param data a function to return the data (probably a reactive function).
#' @param default_vars character list for the variables to include by default.
#' @param default_dependent_variable the name of the dependent variable, or NULL for none.
#' @param se_factor how many standard errors to plot.
#' @importFrom stats as.formula cor median
#' @importFrom scales percent
#' @importFrom GGally ggpairs
#' @import shiny
#' @export
#' @rdname clav-shiny
cluster_module <- function(id,
						   data,
						   default_vars = names(data())[sapply(data(), function(x) { is.numeric(x) })],
						   default_dependent_variable = NULL,
						   se_factor = 1) {
	moduleServer(id, function(input, output, session) {
		get_data_raw <- shiny::reactive({
			if(is.reactive(data)) {
				data()
			} else {
				data
			}
		})

		get_data <- shiny::reactive({
			req(input$variable_selection)
			req(input$dependent_variable)
			get_data_raw() |>
				dplyr::select(dplyr::any_of(c(input$variable_selection, input$dependent_variable))) |>
				stats::na.omit() |>
				dplyr::mutate_if(is.numeric, scale_this)
		})

		get_cluster_fun <- shiny::reactive({
			if(input$clustering_algo == 'kmeans') {
				return(kmeans)
			} else if(input$clustering_alog == 'hclust2') {
				return(hclust2)
			} else {
				stop('Undefined clutering method.')
			}
		})

		get_cluster_fit <- shiny::reactive({
			req(input$k)
			req(input$variable_selection)

			# TODO: Allow for other clustering algorithms other than k-means
			thedata <- get_data()
			fit <- NULL
			if(ncol(thedata) > 0 & nrow(thedata) > 20) {
				# TODO: Need to standardize
				fit <- get_cluster_fun()(thedata[,input$variable_selection], input$k)
			}
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
				inputId = shiny::NS(id, id ='variable_selection'),
				label = 'Variables to include',
				choices = names(get_data_raw()),
				multiple = TRUE,
				selected = default_vars
			)
		})

		output$dependent_variable_ui <- shiny::renderUI({
			shiny::selectInput(
				inputId = shiny::NS(id, id = 'dependent_variable'),
				label = 'Dependent variable',
				choices = c('None', names(get_data_raw())),
				multiple = FALSE,
				selected = ifelse(is.null(default_dependent_variable),
								  'None',
								  default_dependent_variable)
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
							  label = scales::percent(prop.table(after_stat(count))))) +
				geom_bar() +
				geom_text(stat = 'count',
						  position = position_dodge(.9),
						  vjust = -0.5,
						  size = 4) +
				ggtitle('Cluster Sizes')
		})

		output$profile_plot <- shiny::renderPlot({
			fit <- get_cluster_fit()
			thedata <- get_data()
			thedata_dep <- NULL
			if(input$dependent_variable != 'None') {
				thedata_dep <- thedata |> dplyr::select(input$dependent_variable)
				thedata <- thedata |> dplyr::select(!input$dependent_variable)
			}
			clusters <- factor(fit$cluster, labels = letters[1:input$k])
			profile_plot(df = thedata,
						 clusters = clusters,
						 df_dep = thedata_dep,
						 cluster_order = input$variable_selection)
		})

		output$cluster_pairs_plot <- shiny::renderPlot({
			fit <- get_cluster_fit()
			thedata <- get_data()
			thedata$cluster <- factor(fit$cluster, labels = letters[1:input$k])

			GGally::ggpairs(
				thedata,
				columns = names(thedata),
				axisLabels = 'show',
				mapping = ggplot2::aes_string(color='cluster'),
				upper = list(continuous = "cor"),
				diag = list(continuous = "densityDiag", discrete = 'barDiag',
							ggplot2::aes_string(group = 'cluster', fill = 'cluster')),
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
			tab <- describe_by(thedata[,c(input$dependent_variable, 'cluster'),drop=TRUE],
							   group = 'cluster')
			p <- ggplot(thedata, aes_string(x = 'cluster', y = input$dependent_variable)) +
				geom_boxplot() +
				geom_errorbar(data = tab, aes(x = group1,
											  y = mean,
											  ymin = mean - se_factor * se,
											  ymax = mean + se_factor* se),
							  color = 'darkgreen', width = 0.5) +
				geom_point(data = tab, aes(x = group1, y = mean), color = 'blue', size = 2)
			# ggsave(filename = 'test.png', plot = p)
			return(p)
		})

		output$dependent_table <- shiny::renderTable({
			thedata <- get_data()
			fit <- get_cluster_fit()
			thedata$cluster <- factor(fit$cluster, labels = letters[1:input$k])
			describe_by(thedata[,c(input$dependent_variable, 'cluster'),drop=TRUE],
						group = 'cluster')
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

		# TODO: may async
		optimal_clusters <- reactive({
			thedata <- get_data()
			clav::optimal_clusters(thedata, max_k = 6, cluster_fun = get_cluster_fun())
		})

		output$optimal_clusters_plot <- renderPlot({
			optimal_clusters() |> plot() |> print()
		})

		cluster_validation <- reactive({
			thedata <- get_data()
			clav::cluster_validation(thedata, n_clusters = input$k, cluster_fun = get_cluster_fun())
		})

		output$cluster_validation_plot <- renderPlot({
			cluster_validation() |> plot()
		})

		output$cluster_validation_distribution_plot <- renderPlot({
			cluster_validation() |> plot_distributions()
		})
	})
}
