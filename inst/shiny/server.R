function(input, output, session) {
    get_data <- reactive({
        if(input$data_source == 'wgu') {
            daacs.wgu[daacs.wgu$Treat,]
        } else if(input$data_source == 'ec') {
            daacs.ec[daacs.ec$Treat,]
        } else if(input$data_source == 'ualbany') {
            daacs.ualbany
        }
    })

    cluster_module('daacs',
                   data = get_data,
                   default_vars = default_vars,
                   default_dependent_variable = 'mathTotal')
}
