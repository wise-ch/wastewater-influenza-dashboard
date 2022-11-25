library(shiny)

function(input, output, session) {

  # Control slider dates
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

  # Plotting wastewater measurements
  output$raw_plots <- renderPlot({
    raw <- plot_ww_loads(
      wwtp_to_plot = input$wwtp,
      date_range = input$slider_dates)
    raw
  })

  # Plotting confirmed cases
  output$case_plots <- renderPlot({
    re <- plot_cases(
      wwtp_to_plot = input$wwtp,
      date_range = input$slider_dates)
    re
  })

  # Plotting single catchment Re estimates
  output$re_plots <- renderPlot({
    re <- plot_re(
      data_types = input$data_type,
      wwtp_to_plot = input$wwtp,
      date_range = input$slider_dates)
    re
  })

  # Plotting all catchment Re estimates together
  output$all_catchment_plots <- renderPlot({
    all_re <- plot_all_re(
      data_types = input$data_type_all_catchments,
      date_range = input$slider_dates)
    all_re
  })

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
    includeHTML(path = "texts/about_en-gb.html")
  })

}
