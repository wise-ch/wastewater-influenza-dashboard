library(shiny)
library(patchwork)
#library(ggtext)

# it needs to be defined for both ui and server?
i18n <- Translator$new(translation_json_path = "texts/translations.json")
#i18n$set_translation_language("en-gb") # here you select the default translation to display
# zurich flicker: not present if no observe event language change

Sys.setlocale("LC_TIME", "en_GB.UTF-8")
function(input, output, session) {
    # update language based on setting --------
    observeEvent(input$lang, {
        # Here is where we update language in session
        shiny.i18n::update_lang(session, input$lang)
        lang_ref <- c("en-gb" = "en_GB.UTF-8",
                      "de-ch" = "de_CH.UTF-8",
                      "fr-ch" = "fr_CH.UTF-8",
                      "it-ch" = "it_CH.UTF-8")
        Sys.setlocale("LC_TIME", lang_ref[[input$lang]])

    })
    # update title translation
    output$title_panel = renderText({
        i18n$t('Catchments')
    })
    
    output$data_type <- renderUI({
        options_enabled <- c('Wastewater', 'Confirmed (Canton)')
        names(options_enabled) <- i18n$t(c('Wastewater', 'Confirmed cases (in canton)'))

        checkboxGroupInput(inputId = "data_type",
                           label = i18n$t("Data Source (select to compare):"),
                           choices = options_enabled,
                           #"Deaths" = "Deaths",
                           #"Hospitalized patients"= "Hospitalized patients"),
                           selected = "Wastewater")
    })
    
    output$catchment <- renderUI({
        options_catchment<- c('Confirmed (Catchment)')
        names(options_catchment) <- i18n$t("Confirmed cases (in catchment area)")
        
        checkboxGroupInput(inputId = 'catchment_selection',
                           label = NULL,
                           choices = options_catchment)

    })
    
    output$disabled <- renderUI({
        options_disabled <- c('Deaths', 'Hospitalized patients')
        names(options_disabled) <- i18n$t(c('Deaths*', 'Hospitalized patients*'))
        
        disabled(checkboxGroupInput(inputId = "data_type_disabled",
                           label = NULL,
                           choices = options_disabled))
        
    })

    # control slider dates --------

    # what is causing the problem of selection
    # observe({
    # })

    # for Chur, no catchment selection ------
    observeEvent(input$region, {
        # Control the value, min, max according to region selected
        # min according to ww data min for catchment area
        date_range <- c(range((ww_data %>% filter(region == input$region) %>%
                                   select(date))[["date"]])[1],
                        Sys.Date())
        # this is what is causing flickering for region
        updateSliderInput(session, "slider_dates", value = date_range,
                         min = date_range[1], max = Sys.Date())

        if(input$region == 'GR'){
            shinyjs::disable(id = "catchment_selection")
        }else{
            shinyjs::enable(id = "catchment_selection")
        }
    })

    # Plotting cases -------
    output$case_plots <- renderPlot(
        {
            # from all the case plots, it picks region
            # as per drop down menu
            case <- case_plotter(case_data, input$region, input$slider_dates, i18n)
            case
        }
    )
    # Hover info
    output$hover_info_case <- renderUI({
        hover_case <- input$plot_hover_case
        point <- nearPoints(case_data %>% filter(region == input$region) %>% 
                                filter(date >= input$slider_dates[1] & date <= input$slider_dates[2]),
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
                          "<b>", i18n$t("Confirmed cases"),"</b>: ", round(point$cases, 2), "<br/>")))
        )
    })
    # Plotting raw RNA copies -------
    output$raw_plots <- renderPlot(
        {
            raw <- raw_plotter(ww_data, input$region, input$slider_dates, i18n)
            raw
        }
    )
    
    output$hover_info_raw <- renderUI({
        hover_raw <- input$plot_hover_raw
        point <- nearPoints(ww_data %>% filter(region == input$region) %>% mutate(n1 = n1/10^12) %>%
                                filter(date >= input$slider_dates[1] & date <= input$slider_dates[2]),
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
                          "<b>", i18n$t("Gene copies"),"</b> (x10<sup>12</sup>): ", round(point$n1, 2), "<br/>",
                          "<i>(", i18n$t(as.character(point$quantification_flag)), ")</i>")))
        )
    })
    # Plotting Rww+Re for other sources --------
    output$re_plots <- renderPlot(
        {
            if (input$region == "FR") {
                re <- re_plotter2(c(input$data_type, input$catchment_selection), input$region, input$slider_dates, i18n) # call plotter 2: 2 cantons!
            }
            else {
                re <- re_plotter(source = c(input$data_type, input$catchment_selection), canton = input$region, 
                                date_range =  input$slider_dates, i18n = i18n)
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
            source <- c(input$data_type, input$catchment_selection)
            source_canton <- source[source %in% c('Confirmed (Canton)')]
            source_without_canton <- source[! source %in% c('Confirmed (Canton)')]
            
            bern_confirmed <- plotData %>% filter(region == "BE") %>%
                filter(data_type == "Confirmed (Canton)") %>%
                mutate(data_type = recode_factor(data_type, 'Confirmed (Canton)' = 'Confirmed (Bern)'))
            
            fribourg_confirmed <- plotData %>% filter(region == "FR") %>%
                filter(data_type == "Confirmed (Canton)") %>%
                mutate(data_type = recode_factor(data_type, 'Confirmed (Canton)' = 'Confirmed (Fribourg)'))
            
            new_data <- plotData %>% filter(region %in% c("BE", "FR")) %>%
                filter(data_type %in% source_without_canton) %>% 
                filter(date >= input$slider_dates[1] & date <= input$slider_dates[2])
            
            if (length(source_canton)>0) {
                new_data <- new_data %>% bind_rows(bern_confirmed) %>% bind_rows(fribourg_confirmed)
            }
            
            point <- nearPoints(new_data,
                                hover, threshold = 5, maxpoints = 1, addDist = TRUE)
        }
        else {
            
            point <- nearPoints(plotData %>% filter(region == input$region) %>%
                                    filter(data_type %in% c(input$data_type, input$catchment_selection)) %>% 
                                    filter(date >= input$slider_dates[1] & date <= input$slider_dates[2]),
                                hover, threshold = 5, maxpoints = 1, addDist = TRUE)
        }
        
        if (nrow(point) == 0) return(NULL)
        
        left_px <- hover$coords_css$x
        top_px <- hover$coords_css$y
        
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.9); ",
                        "left:", left_px+2, "px; top:", top_px+2, "px;")
        
        point_data_type <- as.character(point$data_type)
        point_data_type <- gsub("Confirmed", "Cases", point_data_type)
        translated_source <- i18n$t(point_data_type)
        
        # actual tooltip created as wellPanel
        wellPanel(
            style = style,
            p(HTML(paste0("<i>", point$date, "</i>", "<br/>",
                          "<b> R<sub>e</sub>: </b>", round(point$median_R_mean, 2),
                          " (", round(point$median_R_lowHPD, 2),", ", round(point$median_R_highHPD, 2), ")",
                          "<br/>",
                          "<i>(", translated_source, ")</i>")))
        )
    })
    
    # plotting all Rww ---------
    output$rww_plots <- renderPlot(
        {
            rww <- canton_plotter(source = 'Wastewater', canton = input$canton, 
                                  date_range = input$slider_dates_cantonal, i18n)
            title_p1 <- i18n$t("Estimated Wastewater R")
            title_p2 <- i18n$t("for different catchment areas")
            title_p3 <- ""
            if (nchar(title_p1)>25) {
                title_p1 <- "R"
                title_p3 <- gsub("R","",title_p1)
            }
            
            rww + theme(legend.position = "none") +
                ggtitle(bquote(.(title_p1)['e']*.(title_p3)~.(title_p2)))
        }
    )
    
    output$hover_info_rww <- renderUI({
        ref <- c("ZH"="Zurich" ,  "GE"="Geneva",
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
            rcc <- canton_plotter(canton = input$canton, source = 'Confirmed (Catchment)', 
                                  date_range = input$slider_dates_cantonal, i18n = i18n)
            title_p1 <- i18n$t("Estimated R")
            title_p2 <- i18n$t(" using catchment specific confirmed cases for different catchment areas")
            title_p3 <- ""
            if (nchar(title_p1)<10) {
                title_p1 <- "R"
                title_p3 <- gsub("R","",title_p1)
            }
            rcc + ggtitle(bquote(.(title_p1)['e']*.(title_p3)*.(title_p2)))
            
        }
    )
    
    output$hover_info_rcc <- renderUI({
        ref <- c("ZH"="Zurich" ,  "GE"="Geneva",
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
    
    output$chur_catchment_disc <- renderUI({
        p(HTML(paste0(strong('NB: '),
                      i18n$t('The estimated R<sub>e</sub> for confirmed cases in the Chur catchment area is currently not available due to a potential data quality issue.'))),
          style="font-size: 95%;")
    })
    
    output$death_hosp_info <- renderUI({
        p(HTML(i18n$t('*The R<sub>e</sub> for hospitalised patients and deaths are currently not displayed because the low case incidence results in large confidence intervals and low usefulness.')), 
          style="font-size: 95%;")
    })
    
    output$other_disclaimers <- renderUI({
        p(
            tags$ul(style="padding-left:10px;font-size: 95%;",
                    tags$li(HTML(paste0(i18n$t("The R<sub>e</sub> for wastewater is informed by infections in the catchment area, and will correspond best to the R<sub>e</sub> based on confirmed cases from that area. All other R<sub>e</sub> traces show the cantonal results, so there may be some dissonance. For instance, canton Zurich is about 3.4x the size of the catchment area served by the Werdhölzli wastewater treatment plant.")))),
                    tags$li(i18n$t('While Lausanne is also one of the catchment areas being monitored, we have not included it in the dashboard due to data quality issues.') ) ) )
    })
    
    # text below plot for more info ---------
    output$link <- renderUI({
        link <- p(i18n$t("The raw measurements of SARS-CoV-2 in wastewater for this location are available "),
                  a(href = paste0("https://sensors-eawag.ch/sars/",tolower(ref[[input$region]]),".html"), i18n$t("here"), .noWS = "outside"),
                  ".",
                  .noWS = c("after-begin", "before-end"), style="margin-bottom:0;font-size: 95%;")
        lod_loq <- p(i18n$t("**Limit of detection (LOD) represents concentration levels at which SARS-CoV-2 can be reliably detected, while limit of quantification (LOQ) represents concentration levels with prespecified precision of detection. <LOD indicates values below the LOD; >LOD represents values below the LOQ, but above the LOD. These values have higher uncertainty, as the number of gene copies is very low. >LOQ indicates reliable values which are above the LOQ."),
                     style = 'margin-bottom:0;font-size: 95%;')
        
        
        #chur_catchment <- p(strong('NB: '),'The estimated R<sub>e</e> for confirmed cases in the Chur catchment area is
        #                    currently not available due to a potential data quality issue.', style="margin-bottom:0;font-size: 95%;")
        
        exclude_lod <- p(HTML(paste0(strong('NB: '),i18n$t('Wastewater measurements from 02.2021 to 07.03.2021 for '), ref[[input$region]],
                                     i18n$t(' have been excluded due to multiple consecutive measurements falling below the limit of quantification and/or detection, which affects the reliability of the raw measurements and the wastewater R<sub>e</sub> estimates.'))),
                         style="margin-bottom:0;font-size: 95%;")
        
        laupen_2cantons <- p(strong('NB: '),i18n$t('The Laupen catchment area consists of municipalities in both Bern and Fribourg (13 communities from Bern and 12 from Fribourg).'), 
                             style="margin-bottom:0;font-size: 95%;")
        
        if (input$region == 'GR') HTML(paste(link, lod_loq, exclude_lod, sep = ""))
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
            case <- case_plotter(case_data, input$region, input$slider_dates, i18n)
            raw <- raw_plotter(ww_data, input$region, input$slider_dates, i18n)
            if (input$region == "FR") {
                re <- re_plotter2(c(input$data_type, input$catchment_selection), input$region, input$slider_dates, i18n) # call plotter 2: 2 cantons!
            } else {
                re <- re_plotter(c(input$data_type, input$catchment_selection), input$region, input$slider_dates, i18n)
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
    # add the about page --------
    output$about_page <- renderUI({
        # different for the different languages
        name <- paste0('texts/about_',input$lang,'.html')
        includeHTML(path = name)
    })
}
