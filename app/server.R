library(shiny)
library(patchwork)

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

  # Plotting wastewater measurements
  output$raw_plots <- renderPlot({
    raw <- plot_ww_loads(
      wwtp_to_plot = input$wwtp,
      date_range = input$slider_dates)
    raw
  })

  # Plotting confirmed cases
  output$case_plots <- renderPlot({
    cases <- plot_cases(
      wwtp_to_plot = input$wwtp,
      date_range = input$slider_dates)
    cases
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

  # Download the plot ------
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("Catchment_", input$wwtp, ".pdf")
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(file) {
      raw <- plot_ww_loads(
        wwtp_to_plot = input$wwtp,
        date_range = input$slider_dates)
      cases <- plot_cases(
        wwtp_to_plot = input$wwtp,
        date_range = input$slider_dates)
      re <- plot_re(
        data_types = input$data_type,
        wwtp_to_plot = input$wwtp,
        date_range = input$slider_dates)

      p <- patchwork::wrap_plots(raw, cases, re, nrow = 3) +
        plot_annotation(caption = paste0(
          "Generated on: ", Sys.Date(),
          " (by: WISE influenza dashboard)"
        ))
      cairo_pdf(
        filename = file,
        width = 16, height = 12, pointsize = 12, family = "sans", bg = "transparent",
        antialias = "subpixel", fallback_resolution = 300
      )
      plot(p)
      dev.off()
    }
  )

  # add the about page --------
  output$about_page <- renderUI({
    includeHTML(path = "texts/about_en-gb.html")
  })

}
