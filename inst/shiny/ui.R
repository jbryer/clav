shiny::navbarPage(
    title = 'DAACS Profile Analysis',

    shiny::tabPanel(
        title = 'Cluster Analysis',
        shiny::sidebarLayout(
            shiny::sidebarPanel(
                width = 3,
                shiny::selectInput(inputId = 'data_source',
                                   label = 'Data source',
                                   choices = c('Western Governors University' = 'wgu',
                                               'Excelsior College' = 'ec',
                                               'University at Albany' = 'ualbany')),
                n_clusters_input('daacs'),
                cluster_variable_select_intput('daacs'),
                dependent_variable_select('daacs')
            ),
            shiny::mainPanel(
                width = 9,
                n_cluster_message('daacs'),
                shiny::tabsetPanel(
                    shiny::tabPanel(
                        title = 'Clusters',
                        n_cluster_plot('daacs'),
                        shiny::hr(),
                        cluster_size_bar_plot('daacs', height = '300px')
                    ),
                    shiny::tabPanel(
                        title = 'Variable Plots',
                        variable_cluster_plot('daacs', height = '600px')

                    ),
                    shiny::tabPanel(
                        title = 'Pairs Plot',
                        cluster_pairs_plot('daacs', height = '600px')
                    ),
                    shiny::tabPanel(
                        title = 'Cluster Plots',
                        bivariate_cluster_plot('daacs'),
                        discriminant_projection_plot('daacs')
                    ),
                    shiny::tabPanel(
                        title = 'Dependent Variable',
                        dependent_variable_plot('daacs'),
                        dependent_variable_table('daacs'),
                        dependent_null_hypothesis('daacs')
                    )
                )

            )
        )
    )

)
