library(shiny)
library(patchwork)
function(input, output, session) {

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
        date_range <- range((ww_data %>% filter(region == input$region) %>% select(date))[["date"]]) # only look at valid region.
        # NB as some places have chunks cut out... Chur and Lugano.
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
                filter(data_type %in% source_without_canton) %>% filter(date >= date_range[1])

            if (length(source_canton)>0) {
                new_data <- new_data %>% bind_rows(bern_confirmed) %>% bind_rows(fribourg_confirmed)
            }

            point <- nearPoints(new_data,
                                hover, threshold = 5, maxpoints = 1, addDist = TRUE)
        }
        else {

            point <- nearPoints(plotData %>% filter(region == input$region) %>%
                                    filter(data_type %in% input$data_type) %>% filter(date >= date_range[1]),
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
    output$rww_plots <- renderPlot(
        {
            rww <- canton_plotter(source = 'Wastewater', canton = input$canton)
            rww + theme(legend.position = "none") +
                ggtitle(bquote("Estimated Wastewater R"['e']~" for different catchment areas"))
        }
    )

    output$hover_info_rww <- renderUI({
        ref <- c("ZH"="Zurich" ,  "VD"="Lausanne",
                 "SG"="Altenrhein", "GR"="Chur",
                 "FR"="Laupen", "TI"="Lugano")
        hover <- input$plot_hover_rww
        point <- nearPoints(plotData %>% filter(region %in% input$canton) %>%
                                filter(data_type =='Wastewater'), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
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

    output$rcc_plots <- renderPlot(
        {
            rcc <- canton_plotter(canton = input$canton, source = 'Confirmed (Catchment)')
            rcc + ggtitle(bquote("Estimated R"['e']~" using catchment specific confirmed cases for different catchment areas"))
                
        }
    )
    
    output$hover_info_rcc <- renderUI({
        ref <- c("ZH"="Zurich" ,  "VD"="Lausanne",
                 "SG"="Altenrhein", "GR"="Chur",
                 "FR"="Laupen", "TI"="Lugano")
        hover <- input$plot_hover_rcc
        point <- nearPoints(plotData %>% filter(region %in% input$canton) %>%
                                filter(data_type =='Confirmed (Catchment)'), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
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
    
    # text below plot for more info ---------
    output$link <- renderUI({
        link <- p("For this location, the raw measurements of SARS-CoV-2 in wastewater are available ",
                  a(href = paste0("https://sensors-eawag.ch/sars/",tolower(ref[[input$region]]),".html"), "here", .noWS = "outside"),
                  ".",
                  .noWS = c("after-begin", "before-end"), style="margin-bottom:0;font-size: 95%;")
        lod_loq <- p('**<LOD indicates values below the limit of detection.\n
                    >LOD represents values below the limit of quantification, but
                     above the limit of detection. These values have higher uncertainty, as the number of gene copies is
                    very low.\n
                    >LOQ indicates reliable values which are above the limit of quantification.' ,
                     style = 'margin-bottom:0;font-size: 95%;')

        chur_catchment <- p(strong('NB: '),'The estimated R',tags$sub('e'), ' for confirmed cases in the Chur catchment area is
                            currently not available due to a potential data quality issue.', style="margin-bottom:0;font-size: 95%;")

        exclude_lod <- p(strong('NB: '),'Wastewater measurements from 02.2021 to 07.03.2021 for ', ref[[input$region]],
                         ' have been excluded due to multiple consecutive measurements falling below the limit of quantification and/or detection,
                         which affects the reliability of the raw measurements and the wastewater R',tags$sub('e'), ' estimates.',
                         style="margin-bottom:0;font-size: 95%;")

        laupen_2cantons <- p(strong('NB: '),'The Laupen catchment area consists of municipalities in both Bern and Fribourg
                             (13 communities from Bern and 12 from Fribourg).', style="margin-bottom:0;font-size: 95%;")

        if (input$region == 'GR') HTML(paste(link, lod_loq, exclude_lod, chur_catchment, sep = ""))
        else if (input$region == 'FR') HTML(paste(link, lod_loq, laupen_2cantons, sep = ""))
        else if (input$region == 'TI') HTML(paste(link, lod_loq, exclude_lod, sep = ""))
        else HTML(paste(link, lod_loq, sep = ""))
    })

    # download the plot ------
    output$downloadPlot <- downloadHandler(
        filename =  function() {
            paste0("Catchment_", ref[[input$region]], ".pdf")
        },
        # content is a function with argument file. content writes the plot to the device
        content = function(file) {
            case <- case_plotter(case_data, input$region)
            raw <- raw_plotter(ww_data, input$region)
            if (input$region == "FR") {
                re <- re_plotter2(input$data_type, input$region) # call plotter 2: 2 cantons!
            } else {
                re <- re_plotter(input$data_type, input$region)
            }
            p <- patchwork::wrap_plots(case,raw,re, nrow = 3)+
                plot_annotation(caption = paste0('Generated on: ',Sys.Date(),
                                                 ' (by: ibz-shiny.ethz.ch/wastewaterRe)'))
            cairo_pdf(filename = file,
                      width = 16, height = 12, pointsize = 12, family = "sans", bg = "transparent",
                      antialias = "subpixel",fallback_resolution = 300)
            plot(p)
            dev.off()  # turn the device off

        }
    )

}
