library(shiny)

function(input, output) {
    
    # keeps track of reactivity - re-computes when input changes
    # use input values when you make your output. Access with $ and Id
    # this value changes as the input bar/slider/button changes. Reactive.
    # Plotting cases -------
    output$case_plots <- renderPlot(
        {
            # from all the case plots, it picks region
            # as per drop down menu
            case <- case_plotter(case_data, input$region)
            case 
        }
    )
    # Hover info
    output$hover_info_case <- renderUI({
        hover_case <- input$plot_hover_case
        point <- nearPoints(case_data %>% filter(region == input$region),
                            hover_case, threshold = 4, maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0) return(NULL)
        
        left_px <- hover_case$coords_css$x
        top_px <- hover_case$coords_css$y
        
        # create style property for tooltip
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
    # Plotting raw RNA copies -------
    output$raw_plots <- renderPlot(
        {
            raw <- raw_plotter(ww_data, input$region)
            raw 
        }
    )
    
    output$hover_info_raw <- renderUI({
        hover_raw <- input$plot_hover_raw
        point <- nearPoints(ww_data %>% filter(region == input$region) %>% mutate(n1 = n1/10^13),
                            hover_raw, threshold = 8, maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0) return(NULL)

        left_px <- hover_raw$coords_css$x
        top_px <- hover_raw$coords_css$y
        
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.9); ",
                        "left:", left_px+2, "px; top:", top_px+2, "px;")
        
        # actual tooltip created as wellPanel
        wellPanel(
            style = style,
            p(HTML(paste0("<i>", point$date, "</i>", "<br/>",
                          "<b> Gene copies</b> (x10<sup>13</sup>): ", round(point$n1, 2), "<br/>",
                          "<i>(", point$quantification_flag, ")</i>")))
        )
    })
    # Plotting Rww+Re for other sources --------
    output$re_plots <- renderPlot(
        {
            re <- re_plotter(input$data_type, input$region)
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
        
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.9); ",
                        "left:", left_px+2, "px; top:", top_px+2, "px;")
        
        # actual tooltip created as wellPanel
        wellPanel(
            style = style,
            p(HTML(paste0("<i>", point$date, "</i>", "<br/>",
                          "<b> R<sub>e</sub>: </b>", round(point$median_R_mean, 2), 
                          " (", round(point$median_R_lowHPD, 2),", ", round(point$median_R_highHPD, 2), ")",
                          "<br/>",
                          "<i>(", point$data_type, ")</i>")))
        )
    })
    
    # plotting all Rww ---------
    # Plotting Rww+Re for other sources --------
    output$rww_plots <- renderPlot(
        {
            rww <- rww_plotter(canton = input$canton)
            rww
        }
    )
    
    output$hover_info_rww <- renderUI({
        ref <- c("ZH"="Zurich" ,  "VD"="Lausanne",
                 "SG"="Altenrhein", "GR"="Chur",
                 "FR"="Laupen", "TI"="Lugano")
        hover <- input$plot_hover
        point <- nearPoints(plotData %>% filter(region %in% input$canton) %>%
                                filter(data_type %in% input$data_type), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
        if (nrow(point) == 0) return(NULL)
        
        left_px <- hover$coords_css$x
        top_px <- hover$coords_css$y
        
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.9); ",
                        "left:", left_px+2, "px; top:", top_px+2, "px;")
        
        # actual tooltip created as wellPanel
        wellPanel(
            style = style,
            p(HTML(paste0("<i>", point$date, "</i>", "<br/>",
                          "<b> R<sub>e</sub>: </b>", round(point$median_R_mean, 2), 
                          " (", round(point$median_R_lowHPD, 2),", ", round(point$median_R_highHPD, 2), ")",
                          "<br/>",
                          "<i>(", ref[[point$region]], ")</i>")))
        )
    })
    
    # text: link to respective EAWAG page
    output$link <- renderUI({
        str1 <- p("The raw measurements of SARS-CoV-2 in wastewater are available ",
                  a(href = paste0("https://sensors-eawag.ch/sars/",tolower(ref[[input$region]]),".html"), "here", .noWS = "outside"),
                  ".",
                  .noWS = c("after-begin", "before-end"))
        
        str2 <- p('*The estimated R',tags$sub('e'), ' for confirmed cases in the catchment area for Chur is 
                            currently not shown due to a potential data quality issue.')

        if (input$region == 'GR') HTML(paste(str1, str2, sep = ""))
        else str1
    })
}