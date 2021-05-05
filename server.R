library(shiny)


#### Define server logic ####
function(input, output) {
    
    # Save an object to the hist element of the output list (same as outputId).
    # Same name as in string in ui output (gets placed in the plot named "hist)
    # render functions: 
    # work with output function to place an R object as HTML on shiny webpage
    # render___ - type of object to build. Within it, we have a code block
    # also keeps track of reactivity - re-computes when input changes
    # use input values when you make your output. Access with $ and Id
    # this value changes as the input bar/slider/button changes. Reactive.

    
    # output$plots <- renderPlot(
    #     {
    #         # from all the raw plots, it picks region
    #         # as per drop down menu
    #         raw <- raw_plotter(ww_data, input$region)
    #         re <- re_plotter(ww_data, input$data_type, input$region)
    #         raw + re + plot_layout(ncol = 1)
    #         #raw_plotly <- ggplotly(raw, tooltip = 'y')
    #         #re_plotly <- ggplotly(re, tooltip = 'y')
    #         #subplot(raw_plotly, re_plotly, nrows = 1)
    #         #re_plotly
    #         #re_plotly
    #     }#, height = 600 # not sure if good idea to fix height.
    # )
    output$case_plots <- renderPlot(
        {
            # from all the case plots, it picks region
            # as per drop down menu
            case <- case_plotter(ww_data, input$region)
            #re <- re_plotter(ww_data, input$data_type, input$region)
            case #+ re + plot_layout(ncol = 1)
            
        }
    )
    
    output$hover_info_case <- renderUI({
        hover_case <- input$plot_hover_case
        point <- nearPoints(ww_data %>% filter(region == input$region),
                            hover_case, threshold = 4, maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0) return(NULL)
        
        left_px <- hover_case$coords_css$x
        top_px <- hover_case$coords_css$y
        
        # create style property fot tooltip
        # background color is set so tooltip is a bit transparent
        # z-index is set so we are sure are tooltip will be on top
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.9); ",
                        "left:", left_px+2, "px; top:", top_px+2, "px;")
        
        # actual tooltip created as wellPanel
        wellPanel(
            style = style,
            p(HTML(paste0("<i>", point$date, "</i>", "<br/>",
                          "<b> Confirmed cases</b>: ", round(point$cases, 2), "<br/>")))
        )
    })
    
    
    output$raw_plots <- renderPlot(
        {
            # from all the raw plots, it picks region
            # as per drop down menu
            raw <- raw_plotter(ww_data, input$region)
            #re <- re_plotter(ww_data, input$data_type, input$region)
            raw #+ re + plot_layout(ncol = 1)

        }
    )
    
    output$hover_info_raw <- renderUI({
        hover_raw <- input$plot_hover_raw
        point <- nearPoints(ww_data %>% filter(region == input$region) %>% mutate(n1 = n1/10^13),
                            hover_raw, threshold = 8, maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0) return(NULL)

        left_px <- hover_raw$coords_css$x
        top_px <- hover_raw$coords_css$y
        
        # create style property fot tooltip
        # background color is set so tooltip is a bit transparent
        # z-index is set so we are sure are tooltip will be on top
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.9); ",
                        "left:", left_px+2, "px; top:", top_px+2, "px;")
        
        # actual tooltip created as wellPanel
        wellPanel(
            style = style,
            p(HTML(paste0("<i>", point$date, "</i>", "<br/>",
                          "<b> Gene copies</b> (x10<sup>13</sup>): ", round(point$n1, 2), "<br/>")))
        )
    })
    
    output$re_plots <- renderPlot(
        {
            # from all the raw plots, it picks region
            # as per drop down menu
            #raw <- raw_plotter(ww_data, input$region)
            re <- re_plotter(input$data_type, input$region)
            #raw + re + plot_layout(ncol = 1)
            re

        }
    )
    
    output$hover_info_re <- renderUI({
        hover <- input$plot_hover_re
        point <- nearPoints(plotData %>% filter(region == input$region) %>%
                                filter(data_type %in% input$data_type), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0) return(NULL)
   
        left_px <- hover$coords_css$x
        top_px <- hover$coords_css$y
        
        # create style property fot tooltip
        # background color is set so tooltip is a bit transparent
        # z-index is set so we are sure are tooltip will be on top
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.9); ",
                        "left:", left_px+2, "px; top:", top_px+2, "px;")
        
        # actual tooltip created as wellPanel
        wellPanel(
            style = style,
            p(HTML(paste0("<i>", point$date, "</i>", "<br/>",
                          "<b> R<sub>e</sub>: </b>", round(point$median_R_mean, 2), 
                          " (", round(point$median_R_lowHPD, 2),", ", round(point$median_R_highHPD, 2), ")",
                          "<br/>",
                          "(", point$data_type, ")")))
        )
    })
    
    
    
}
