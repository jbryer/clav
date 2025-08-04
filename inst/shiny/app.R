library(clav)
library(shiny)
library(ggplot2)
library(GGally)

# If there are no data.frames in the environment we will load some.
if(!any(sapply(ls(), FUN = function(x) { is.data.frame(get(x)) }))) {
    # Load the PISA data and separate into separate data.frames by country
    data("pisa2015", package = "clav")
    pisa_usa <- pisa2015 |> dplyr::filter(country == 'UNITED STATES')
    pisa_canada <- pisa2015 |> dplyr::filter(country == 'CANADA')
    # data("daacs", package = "clav")
    # daacs <- daacs
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

server <- clav_shiny_server
ui <- clav_shiny_ui

app_env <- new.env()
assign('data_frames', data_frames, app_env)

environment(server) <- as.environment(app_env)
environment(ui) <- as.environment(app_env)

##### Run the Shiny application ################################################
shiny::shinyApp(ui = ui, server = server)
