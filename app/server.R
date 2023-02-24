library(shiny)
library(patchwork)

server = function(input, output, session) {

  # Plotting wastewater measurements
  output$raw_plots <- renderPlot({
    validate(need(
      input$measuring_periods != "",
      "Please select at least one season in the menu to generate the plot."))
    raw <- plot_ww_loads(
      wwtp_to_plot = input$wwtp,
      measuring_period = input$measuring_periods,
      disease_classes = input$influenza_type)
    raw
  })

  # Plotting confirmed cases
  output$case_plots <- renderPlot({
    validate(need(
      input$measuring_periods != "",
      "Please select at least one season in the menu to generate the plot."))
    cases <- plot_cases(
      wwtp_to_plot = input$wwtp,
      measuring_period = input$measuring_periods, 
      disease_classes = input$influenza_type)
    cases
  })

  # Plotting single catchment Re estimates
  output$re_plots <- renderPlot({
    validate(need(
      input$data_type != "",
      "Please select at least one data source in the menu to generate the plot."))
    validate(need(
      input$measuring_periods != "",
      "Please select at least one season in the menu to generate the plot."))
    re <- plot_re(
      data_types = input$data_type,
      wwtp_to_plot = input$wwtp,
      measuring_period = input$measuring_periods,
      disease_classes = input$influenza_type)
    re
  })

  # Plotting all catchment Re estimates together
  output$all_catchment_plots <- renderPlot({
    validate(need(
      input$data_type_all_catchments != "",
      "Please select at least one data source in the menu to generate the plot."))
    all_re <- plot_all_re(
      data_types = input$data_type_all_catchments,
      measuring_period = input$measuring_period_all_catchments,
      disease_classes = input$influenza_type_all_catchments)
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
        measuring_periods = input$measuring_periods,
        disease_classes = input$influenza_type)
      cases <- plot_cases(
        wwtp_to_plot = input$wwtp,
        measuring_periods = input$measuring_periods,
        disease_classes = input$influenza_type)
      re <- plot_re(
        data_types = input$data_type,
        wwtp_to_plot = input$wwtp,
        measuring_periods = input$measuring_periods, 
        disease_classes = input$influenza_type)

      p <- patchwork::wrap_plots(raw, cases, re, nrow = 3) +
        plot_annotation(caption = paste0(
          "Generated on: ", Sys.Date(),
          " (by: WISE influenza dashboard)"
        ))
      pdf(
        file = file,
        width = 16, height = 12, family = "sans"
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

