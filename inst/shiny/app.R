library(clav)
library(shiny)
library(ggplot2)
library(GGally)

# source('../../R/shiny_module.R')

# If there are no data.frames in the environment we will laod some.
if(!any(sapply(ls(), FUN = function(x) { is.data.frame(get(x)) }))) {
    # Load the PISA data and separate into separate data.frames by country
    data("pisa2015", package = "clav")
    pisa_usa <- pisa2015 |> dplyr::filter(country == 'UNITED STATES')
    pisa_canada <- pisa2015 |> dplyr::filter(country == 'CANADA')
}

# data_frames is a list of available data sets in the Shiny application. By
# it will use any data.frames in the global environment. This can be modified
# for your specific data set.
data_frames <- list()
for(i in ls()) {
    if(is.data.frame(get(i))) {
        data_frames[[i]] <- get(i)
    }
}

# se_bar_multiplier <- 1

##### Shiny UI #################################################################
shiny_ui <- shiny::fluidPage(
    shiny::titlePanel('Cluster Analysis'),
    shiny::sidebarLayout(
        shiny::sidebarPanel(
            width = 3,
            shiny::uiOutput('data_frame_select'),
            n_clusters_input('shinyclav'),
            cluster_variable_input('shinyclav'),
            dependent_variable_input('shinyclav')
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

##### Shiny Server #############################################################
shiny_server <- function(input, output, session) {
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

##### Run the Shiny application ################################################
shiny::shinyApp(ui = shiny_ui, server = shiny_server)
