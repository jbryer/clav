shinyServer(function(input, output, session) {
	get_data <- reactive({
		req(input$dataset)
		if(input$dataset == 'penguins') {
			return(penguins)
		} else if(input$dataset == 'iris') {
			return(iris)
		} else if(input$dataset == 'daacs') {
			return(daacs)
		}
	})

	get_cluster_vars <- reactive({
		data_vars[[input$dataset]]$cluster_vars
	})

	get_cluster_var <- reactive({
		data_vars[[input$dataset]]$cluster_var
	})

	##### DAACS ################################################################
	output$daacs_cluster_fit_plot <- renderPlot({
		plot(daacs_oc) |> print()
	})

	output$daacs_cluster_agreement_plot <- renderPlot({
		plot(daacs_af)
	})

	output$daacs_cluster_overlap_plot <- renderPlot({
		plot(daacs_of)
	})

	output$daacs_cluster_validation_plot <- renderPlot({
		plot(daacs_cv[[paste0('k', input$daacs_k)]])
	})

	output$daacs_distributions_plot <- renderPlot({
		plot_distributions(daacs_cv[[paste0('k', input$daacs_k)]])
	})

	output$daacs_profile_plot <- renderPlot({
		daacs_fit <- stats::kmeans(daacs[,daacs_cluster_vars], input$daacs_k)
		clav::profile_plot(df = daacs[,daacs_cluster_vars],
						   clusters = LETTERS[daacs_fit$cluster],
						   df_dep = daacs[,daacs_outcome_vars,drop=FALSE])
	})

	##### Figures ##############################################################
	output$figure1 <- renderPlot({
		# ggpairs(poster_data[,c('mpg', 'wt', 'cyl')])
	})

	optimal_clusters <- reactive({
		optimal_clusters_fits[[input$dataset]]
	})

	output$optimal_clusters_plot <- renderPlot({
		oc <- optimal_clusters()
		plot(oc) |> print()
	})

	output$cluster_overlap_plot <- renderPlot({
		cluster_overlap_fits[[input$dataset]] |> plot()
	})

	output$cluster_agreement_plot <- renderPlot({
		cluster_agreement_fits[[input$dataset]] |> plot() + ylim(0, 1)
	})

	output$cluster_distribution_plot <- renderPlot({
		cluster_agreement_fits[[input$dataset]][[paste0('k', input$k)]]$cv |> plot_distributions()
	})

	output$cluster_validation_plot <- renderPlot({
		cluster_agreement_fits[[input$dataset]][[paste0('k', input$k)]]$cv |> plot()
	})

	output$scatter_plot <- renderPlot({
		thedata <- data_vars[[input$dataset]]$data
		cluster_vars <- data_vars[[input$dataset]]$cluster_vars
		cluster_var <- data_vars[[input$dataset]]$cluster_var
		ggplot(thedata, aes(x = .data[[cluster_vars[1]]], y = .data[[cluster_vars[2]]],
							color = .data[[cluster_var]])) +
			geom_point() +
			coord_equal()
	})

	output$fitted_scatter_plot <- renderPlot({
		thedata <- data_vars[[input$dataset]]$data
		cluster_vars <- data_vars[[input$dataset]]$cluster_vars
		fit <- stats::kmeans(thedata[,cluster_vars], input$k)
		thedata$fitted_cluster <- LETTERS[fit$cluster]
		ggplot(thedata, aes(x = .data[[cluster_vars[1]]], y = .data[[cluster_vars[2]]],
							color = .data[['fitted_cluster']])) +
			geom_point() +
			coord_equal()
	})

	output$results_table <- renderText({
		optimal_k <- readxl::read_excel('data/optimal_k.xlsx')
		optimal_k |>
			huxtable::as_hux() |>
			huxtable::merge_cells(1, 2:3) |>
			huxtable::merge_cells(1, 4:5) |>
			huxtable::merge_cells(3, 2:3) |>
			huxtable::merge_cells(3, 4:5) |>
			huxtable::set_bold(row = nrow(optimal_k) + 1) |>
			# huxtable::set_outer_padding(value = 0) |>
			huxtable::to_html()
	})

	##### Rmarkdown boxes ######################################################
	# output$results <- renderRmd('docs/results.Rmd', mathjax = TRUE)

	##### Table output #########################################################

    ##### Output for the demographics tab ######################################
    output$demographics_primary_input <- renderUI({
    	selectInput(
    		'primary_variable',
    		'Demographic Variable',
    		choices = primary_variables
    	)
    })

    output$demographics_secondary_input <- renderUI({
    	selectInput(
    		'secondary_variable',
    		'Secondary Variable',
    		choices = c('None', secondary_variables)
    	)
    })

    output$demographic_plot <- renderPlot({
    	req(input$primary_variable)
    	req(input$secondary_variable)
    	p <- NULL
    	var1 <- input$primary_variable
    	var2 <- input$secondary_variable
    	if(var2 == 'None') {
	    	if(!is_qualitative(poster_data[,var1])) {
	    		p <- ggplot(poster_data, aes_string(x = var1)) +
	    			geom_density()
	    	} else {
	    		p <- ggplot(poster_data, aes_string(x = var1)) +
	    			geom_bar(alpha = 0.3) +
	    			geom_text(
	    				aes(
	    					label = sprintf(
	    						'%d\n(%s)',
	    						..count..,
	    						pct_format(..count.. / sum(..count..))
	    					)
	    				),
	    				stat = 'count',
	    				hjust = 1,
	    				colour = 'black',
	    				size = 5) +
	    			coord_flip()
	    	}
    	} else {
    		if(is_qualitative(poster_data[,var1]) &
    		   is_qualitative(poster_data[,var2])) {
    			# Two qualitative variables: bar plot
    			p <- ggplot(poster_data, aes_string(x = var1, fill = var2)) +
    				geom_bar(position = 'dodge') +
    				geom_text(
    					aes(label = sprintf('%d', ..count..)),
    					stat = 'count',
    					hjust = 1,
    					colour = 'black',
    					position = position_dodge(width = .9),
    					size = 5) +
    				coord_flip()
    		} else if(!is_qualitative(poster_data[,var1]) &
    				  !is_qualitative(poster_data[,var2])) {
    			# Two quantitative variables: scatter plot
    			p <- ggplot(poster_data, aes_string(x = var1, y = var2)) +
    				geom_point()
    		} else {
    			# Make sure var1 is quantitative, var2 is qualitative
    			if(is_qualitative(poster_data[,var1])) {
    				var2 <- input$primary_variable
    				var1 <- input$secondary_variable
    			}
    			# One qualitative, one qualitative: Box plot
    			p <- ggplot(poster_data, aes_string(x = var2, y = var1)) +
    				geom_boxplot()
    		}
    	}
    	return(p)
    })

    output$demographic_table <- render_gt({
    	# TODO:
    })

    ##### Change background color for certain tabs #############################
    observeEvent(input$shiny_poster, {
    	if(input$shiny_poster %in% tabs_with_white_background){
    		session$sendCustomMessage("background-color", "white")
    	} else {
    		session$sendCustomMessage("background-color", background_color)
    	}
    })

})
