library(shiny)

function(input, output) {
    
    # keeps track of reactivity - re-computes when input changes
    # use input values when you make your output. Access with $ and Id
    # this value changes as the input bar/slider/button changes. Reactive.
    observeEvent(input$btn, {
        shinyjs::disable(selector = "#variable input[value='Deaths']")
    })
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
            if (input$region == "FR") {
                re <- re_plotter2(input$data_type, input$region) # call plotter 2: 2 cantons!
            }
            else {
                re <- re_plotter(input$data_type, input$region)
            }
            re
        }
    )
    
    output$hover_info_re <- renderUI({
        hover <- input$plot_hover_re
        # special treatment: Laupen - has both Fribourg and Bern! ------
        if (input$region == "FR") {
            source <- input$data_type
            source_canton <- source[source %in% c('Confirmed (Canton)')]
            source_without_canton <- source[! source %in% c('Confirmed (Canton)')]
            
            bern_confirmed <- plotData %>% filter(region == "BE") %>%
                filter(data_type == "Confirmed (Canton)") %>%
                mutate(data_type = recode_factor(data_type, 'Confirmed (Canton)' = 'Confirmed (Bern)'))
            
            fribourg_confirmed <- plotData %>% filter(region == "FR") %>%
                filter(data_type == "Confirmed (Canton)") %>%
                mutate(data_type = recode_factor(data_type, 'Confirmed (Canton)' = 'Confirmed (Fribourg)'))
            
            new_data <- plotData %>% filter(region %in% c("BE", "FR")) %>%
                filter(data_type %in% source_without_canton)
            
            if (length(source_canton)>0) {
                new_data <- new_data %>% bind_rows(bern_confirmed) %>% bind_rows(fribourg_confirmed)
            }
            
            point <- nearPoints(new_data, 
                                hover, threshold = 5, maxpoints = 1, addDist = TRUE)
        }
        else {
            point <- nearPoints(plotData %>% filter(region == input$region) %>%
                                    filter(data_type %in% input$data_type), 
                                hover, threshold = 5, maxpoints = 1, addDist = TRUE)
        }
        
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
    
    # text: link to respective EAWAG page---------
    output$link <- renderUI({
        str1 <- p("The raw measurements of SARS-CoV-2 in wastewater are available ",
                  a(href = paste0("https://sensors-eawag.ch/sars/",tolower(ref[[input$region]]),".html"), "here", .noWS = "outside"),
                  ".",
                  .noWS = c("after-begin", "before-end"), style="margin-bottom:0;")
        
        str2 <- p(tags$sup('†'),'The estimated R',tags$sub('e'), ' for confirmed cases in the catchment area for Chur is 
                            currently not available due to a potential data quality issue.')

        str3 <- p(tags$sup('†'),'The Laupen catchment area consists of areas from both Bern and Fribourg (13 communities from Bern and 12 from Fribourg).')
        
        if (input$region == 'GR') HTML(paste(str1, str2, sep = ""))
        else if (input$region == 'FR') HTML(paste(str1, str3, sep = ""))
        else str1
    })
}