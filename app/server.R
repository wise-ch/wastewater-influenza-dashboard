library(shiny)

# zurich flicker: not present if no observe event language change

Sys.setlocale("LC_TIME", "en_GB.UTF-8")
function(input, output, session) {
  
  # Update title translation
  output$title_panel <- renderText({
    "Catchments"
  })

  # Update available data types based on pathogen --------
  observeEvent(input$wwtp, {

    output$data_type <- renderUI({
      checkboxGroupInput(
        inputId = "data_type",
        label = "Data source (select to compare):",
        choices = c("Wastewater", "Confirmed cases"),
        selected = "Wastewater"
      )
    })

    # output$disabled_data_types <- renderUI({
    #   disabled(checkboxGroupInput(
    #     inputId = "data_type_disabled",
    #     label = NULL,
    #     choices = options_disabled
    #   ))
    # })
  })

  # Control slider dates --------
  observeEvent(input$wwtp, {
    # Control the value, min, max according to region selected
    ww_loads_filtered <- ww_loads %>% filter(wwtp == input$wwtp, !is.na(sample_date))
    date_range <- range(ww_loads_filtered$sample_date)
    # this is what is causing flickering for region
    updateSliderInput(session, "slider_dates",
      value = date_range,
      min = date_range[1], max = Sys.Date()
    )
  })
# 
#   # Plotting cases -------
#   output$case_plots <- renderPlot({
#     # from all the case plots, it picks region
#     # as per drop down menu
#     p <- case_plotter(data = ww_loads, wwtp = input$wwtp, date_range = input$slider_dates)
#     p
#   })

  # Plotting raw RNA copies -------
  output$raw_plots <- renderPlot({
    raw <- plot_ww_loads(wwtp_to_plot = input$wwtp, date_range = input$slider_dates)
    raw
  })

  # output$hover_info_raw <- renderUI({
  #   req(input$pathogen)
  #   hover_raw <- input$plot_hover_raw
  # 
  #   select_data <- plotDataWW %>%
  #     mutate(observation = observation / 10^12) %>%
  #     filter(region == input$region, pathogen_type == input$pathogen) %>%
  #     filter(date >= input$slider_dates[1] & date <= input$slider_dates[2])
  # 
  #   point <- nearPoints(select_data,
  #     hover_raw,
  #     threshold = 8, maxpoints = 1, addDist = TRUE,
  #     xvar = "date", yvar = "observation"
  #   )
  #   if (nrow(point) == 0) {
  #     return(NULL)
  #   }
  # 
  #   left_px <- hover_raw$coords_css$x
  #   top_px <- hover_raw$coords_css$y
  # 
  #   style <- paste0(
  #     "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.9); ",
  #     "left:", left_px + 2, "px; top:", top_px + 2, "px;"
  #   )
  # 
  #   # actual tooltip created as wellPanel
  #   wellPanel(
  #     style = style,
  #     p(HTML(paste0(
  #       "<i>", point$date, "</i>", "<br/>",
  #       "<b>", i18n$t("Gene copies"), "</b> (x10<sup>12</sup>): ", round(point$observation, 2), "<br/>",
  #       "<i>(", i18n$t(as.character(point$quantification_flag)), ")</i>", "<br/>",
  #       "<i>(", "Protocol: ", point$protocol, ")</i>"
  #     )))
  #   )
  # })

  # Plotting Rww+Re for other sources --------
  output$re_plots <- renderPlot({
    re <- plot_re(
      data_types = input$data_type, wwtp_to_plot = input$wwtp,
      date_range = input$slider_dates)
    re
  })

  # output$hover_info_re <- renderUI({
  #   req(input$pathogen)
  #   hover <- input$plot_hover_re
  # 
  #   select_data <- plotDataRe %>%
  #     filter(region %in% catchment_to_cantons[[input$region]]) %>%
  #     filter(pathogen_type == input$pathogen) %>%
  #     filter(data_type %in% input$data_type) %>%
  #     filter(date >= input$slider_dates[1] & date <= input$slider_dates[2])
  # 
  #   point <- nearPoints(select_data, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
  #   if (nrow(point) == 0) {
  #     return(NULL)
  #   }
  # 
  #   left_px <- hover$coords_css$x
  #   top_px <- hover$coords_css$y
  # 
  #   style <- paste0(
  #     "position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.9); ",
  #     "left:", left_px + 2, "px; top:", top_px + 2, "px;"
  #   )
  # 
  #   point_data_type <- as.character(point$data_type)
  #   point_data_type <- gsub("Confirmed", "Cases", point_data_type)
  #   translated_source <- i18n$t(point_data_type)
  # 
  #   # actual tooltip created as wellPanel
  #   wellPanel(
  #     style = style,
  #     p(HTML(paste0(
  #       "<i>", point$date, "</i>", "<br/>",
  #       "<b> R<sub>e</sub>: </b>", round(point$median_R_mean, 2),
  #       " (", round(point$median_R_lowHPD, 2), ", ", round(point$median_R_highHPD, 2), ")",
  #       "<br/>",
  #       "<i>(", translated_source, ")</i>"
  #     )))
  #   )
  # })

  # # download the plot ------
  # output$downloadPlot <- downloadHandler(
  #   filename = function() {
  #     paste0("Catchment_", canton_to_catchment[[input$region]], ".pdf")
  #   },
  #   # content is a function with argument file. content writes the plot to the device
  #   content = function(file) {
  #     case <- case_plotter(plotDataObs, input$region, input$pathogen, input$slider_dates)
  #     raw <- raw_plotter(plotDataWW, input$region, input$pathogen, input$slider_dates)
  #     if (input$region == "FR") {
  #       re <- re_plotter2(input$data_type, input$region, input$slider_dates) # call plotter 2: 2 cantons!
  #     } else {
  #       re <- plot_re(re_to_plot, input$data_type, input$region, input$pathogen, input$slider_dates)
  #     }
  #     p <- patchwork::wrap_plots(case, raw, re, nrow = 3) +
  #       plot_annotation(caption = paste0(
  #         "Generated on: ", Sys.Date(),
  #         " (by: ibz-shiny.ethz.ch/wastewaterRe)"
  #       ))
  #     cairo_pdf(
  #       filename = file,
  #       width = 16, height = 12, pointsize = 12, family = "sans", bg = "transparent",
  #       antialias = "subpixel", fallback_resolution = 300
  #     )
  #     plot(p)
  #     dev.off() # turn the device off
  #   }
  # )
  
  # add the about page --------
  output$about_page <- renderUI({
    # different for the different languages
    name <- paste0("texts/about_", input$lang, ".html")
    includeHTML(path = name)
  })

}
