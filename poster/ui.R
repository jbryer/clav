shinyUI(navbarPage(
	id = 'shiny_poster',
	header = '',
	position = 'static-top',
	windowTitle = poster_title,
	title = navbar_title,
	tabPanel(
        'Overview',
        # if(file.exists('www/qrcode.png')) {
        # 	tags$div(
        # 		style = "position: fixed; top: 60px; right: 10px; z-index: 9999;",
        # 		tags$img(src = "qrcode.png", width = "100px") # Place image in www/
        # 	)
        # },
        # tags$div(
        # 	style = "position: fixed; top: 60px; left: 20px; z-index: 9999;",
        # 	tags$img(src = "clav1.png", height = "100px") # Place image in www/
        # ),
        fluidRow( # Title and Authors
        	style = 'position: relative; top: -15px;',
        	column(
        		width = 1,
        		tags$img(src = "clav1.png", height = "100px")
        	),
        	column(
        		width = 10,
        		div(style = 'font-size: 26px; padding-bottom: 10px;',
        			align = 'center',
        			poster_title
        		),
        		div(align = 'center',
        			style = 'padding-bottom: 5px',
        			poster_subtitle
        		),
        		div(align = 'center',
        			style = 'padding-bottom: 5px',
        			poster_authors
        		)
        	),
        	column(
        		width = 1,
        		tags$img(src = "qrcode.png", width = "100px")
        	)
        ),
        fluidRow( # Row 1 (three column layout)
        	column( # Column 1
        		width = 4,
        		style='padding:0px;',
	        	box(title = 'Abstract',
	        		width = 12,
	        		includeMarkdown('docs/abstract.md')
	        	),
        		box(title = 'Research Questions',
        			width = 12,
        			includeMarkdown('docs/research_questions.md')
        		),
        		box(title = 'Optimal clusters based on existing statistics',
        			width = 12,
        			tabsetPanel(
        				tabPanel(
        					'Table',
        					htmlOutput('results_table')
        				),
        				tabPanel(
        					'Plot',
        					plotOutput('optimal_clusters_plot', height = '300px')
        				),
        				tabPanel(
        					'Scatterplot',
        					plotOutput('scatter_plot', height = '200px')
        				)
        			),
        			br(),
        			p('Using six of the more common metrics for determining the optimal number of clusters fails to identify three clusters in nearly all cases.')
        		)
        	),
        	column( # Column 2
        		width = 4,
        		style='padding:0px;',
        		box(title = 'Method',
        			width = 12,
        			includeMarkdown('docs/method.md'),
        			selectInput(
        				inputId = 'dataset',
        				label = 'Select Data Set',
        				choices = c("Palmer's Penguins" = 'penguins',
        							"Edgar Andnerson's Iris" = 'iris'
        							# "DAACS" = 'daacs'
        				),
        				selected = 'penguins')
        		),
        		box(title = 'Bootstrap Distributions',
        			width = 12,
        			sliderInput(inputId = 'k', label = 'k',
        						min = 2, max = 6, value = 3),
        			tabsetPanel(
        				tabPanel(
        					title= 'Distrubtions',
        					plotOutput('cluster_distribution_plot', height = '250px')
        				),
        				tabPanel(
        					title = 'Validation',
        					plotOutput('cluster_validation_plot', height = '250px')
        				),
        				tabPanel(
        					title = 'Scatterplot',
        					plotOutput('fitted_scatter_plot', height = '250px')
        				)
        			)
        		)
        	),
        	column( # Column 3
        		width = 4,
        		style='padding:0px;',
        		box(title = 'Cluster Overlap Fit',
        			width = 12,
        			plotOutput('cluster_overlap_plot', height = '200px')
        		),
        		box(title = 'Cluster Agreement Fit',
        			width = 12,
        			plotOutput('cluster_agreement_plot', height = '200px')
        		),
        		box(title = 'Discussion',
        			width = 12,
        			includeMarkdown('docs/discussion.md'))

        	)
        ),
        fluidRow( # Row 2
        	column( # Column 1
        		width = 4,
        		style='padding:0px;',
        	),
        	column( # Column 2
        		width = 4,
        		style='padding:0px;',
        	),
        	column(
        		width = 4,
        		style='padding:0px',
        	)
        ),
        ##### Added Web Dependencies #############################################
        # Note that these need to be present within a tabPanel so to not create
        # an empty tab that is clickable and therefore blank.
        useShinydashboard(),
        shinyjs::inlineCSS(".navbar .navbar-default .navbar-static-top {margin-bottom: 0px}"),
        fullscreen_all(click_id = "page_full"),
        ##### Set Background Color
        tags$head(
        	tags$script("Shiny.addCustomMessageHandler('background-color', function(color) {
        document.body.style.backgroundColor = color; });
		")
        ),
        ##### Full Screen Button
        tags$script(
        	HTML("var header = $('.navbar > .container-fluid');
                              header.append('<div style=\"float:right; padding-top: 8px\"><button id=\"page_full\" type=\"button\" class=\"btn btn-outline-primary action-button\" onclick=\"page_full()\">View Full Screen</button></div>')")
        )
        ##### End Web Dependencies #############################################
	),
	##### Demographics Tab #####################################################
    # tabPanel(
    #     'Descriptive Stats',
    #     fluidRow(
    #     	column(
    #     		width = 6,
    #     		style='padding:10px;',
    #     		includeMarkdown('docs/descriptive_stats_info.md')
    #     	),
    #     	column(
    #     		width = 3,
    #     		style='padding:10px;',
    #     		uiOutput('demographics_primary_input')
    #     	),
    #     	column(
    #     		width = 3,
    #     		style='padding:10px;',
    #     		uiOutput('demographics_secondary_input')
    #     	)
    #     ),
    #     fluidRow(
    #     	column(
    #     		width = 12,
    #     		style='padding:0px;',
    #     		box(width = 12,
    #     			plotOutput("demographic_plot", height = '600px')
    #     		)
    #     	)
    #     	# TODO: Add demographic table
    #     	# column(
    #     	# 	width = 6,
    #     	# 	style='padding:0px;'
    #     	# 	# box(width = 12)
    #     	# )
    #     )
    # ),
    ##### Paper / About Tab ####################################################
    tabPanel(
        'Paper',
        includeMarkdown('docs/paper.md')
    )
))
