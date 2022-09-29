library(shiny)
library(shiny.i18n)
library(patchwork)
library(ggrepel)

# it needs to be defined for both ui and server?
i18n <- Translator$new(translation_json_path = "texts/translations.json")
#i18n$set_translation_language("en-gb") # here you select the default translation to display
# zurich flicker: not present if no observe event language change

Sys.setlocale("LC_TIME", "en_GB.UTF-8")
function(input, output, session) {
    # Update language based on setting --------
    observeEvent(input$lang, {
        # Here is where we update language in session
        shiny.i18n::update_lang(session, input$lang)
        lang_ref <- c("en-gb" = "en_GB.UTF-8",
                      "de-ch" = "de_CH.UTF-8",
                      "fr-ch" = "fr_CH.UTF-8",
                      "it-ch" = "it_CH.UTF-8")
        Sys.setlocale("LC_TIME", lang_ref[[input$lang]])

    })
    # Update title translation
    output$title_panel = renderText({
        i18n$t('Catchments')
    })
    
    # Update available pathogens based on region --------
    observeEvent(input$region, {
      options_enabled <- c()
      options_disabled <- c()
      
      covid_regions <- c("ZH", "SG", "GR", "FR", "TI", "GE")
      if (input$region %in% covid_regions) {
        options_enabled <- c(options_enabled, "SARS-CoV-2" = "COVID")
      }
      
      flu_regions <- c("ZH", "BS", "GE")
      if (input$region %in% flu_regions) {
        options_enabled <- c(options_enabled, "Influenza A Virus" = "IAV", "Influenza B Virus" = "IBV")
      }
      
      output$pathogen <- renderUI({
        
        selectInput(inputId = "pathogen",
                   label = i18n$t("Select pathogen:"),
                   choices = options_enabled,
                   selected = options_enabled[1])
        
      })
      
    })
    
    # Update available data types based on pathogen --------
    observeEvent(input$pathogen, {
      if (input$pathogen == "COVID") {
        options_enabled <- c(
          "Wastewater",
          "Confirmed (Canton)",
          "Confirmed (Catchment)")
        names(options_enabled) <- c(
          "Wastewater",
          "Confirmed cases (in canton)",
          "Confirmed cases (in catchment area)")
        options_disabled <- c(
          'Deaths',
          'Hospitalized patients')
        names(options_disabled) <- i18n$t(c(
          'Deaths*',
          'Hospitalized patients*'))
      } else if (input$pathogen %in% c("IAV", "IBV")) {
        options_enabled <- c(
          "Wastewater",
          "Influenza-like illness consultations (National)")
        names(options_enabled) <- c(
          "Wastewater",
          "Influenza-like illness consultations (National)")
        options_disabled <- c()
      }
      
      output$data_type <- renderUI({
        
        checkboxGroupInput(inputId = "data_type",
                           label = i18n$t("Data source (select to compare):"),
                           choices = options_enabled,
                           selected = "Wastewater")  # this assumes wastewater is an available data type for all pathogens
      })
      
      output$disabled_data_types <- renderUI({
        
        disabled(checkboxGroupInput(inputId = "data_type_disabled",
                                    label = NULL,
                                    choices = options_disabled))
        
      })
      
    })

    # Control slider dates --------
    observeEvent(input$region, {
        # Control the value, min, max according to region selected
        # min according to ww data min for catchment area
        date_range <- c(range((plotDataWW %>% filter(region == input$region) %>%
                                   select(date))[["date"]])[1],
                        Sys.Date())
        # this is what is causing flickering for region
        updateSliderInput(session, "slider_dates", value = date_range,
                         min = date_range[1], max = Sys.Date())
    })

    # Plotting cases -------
    output$case_plots <- renderPlot(
        {
            # from all the case plots, it picks region
            # as per drop down menu
            case <- case_plotter(data = plotDataObs, canton = input$region, 
                                 pathogen = input$pathogen, date_range = input$slider_dates,
                                 i18n = i18n)
            case
        }
    )
    
    # Hover info
    output$hover_info_case <- renderUI({
        hover_case <- input$plot_hover_case
        select_data <- plotDataObs %>%
          filter(region == input$region, pathogen_type == input$pathogen) %>%
          filter(date >= input$slider_dates[1] & date <= input$slider_dates[2]) 
        point <- nearPoints(select_data,
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
                          "<b>", i18n$t("Observation"),"</b>: ", round(point$observation, 2), "<br/>")))
        )
    })
    
    # Plotting raw RNA copies -------
    output$raw_plots <- renderPlot(
        {
            raw <- raw_plotter(data = plotDataWW, canton = input$region, pathogen = input$pathogen, date_range = input$slider_dates, i18n = i18n)
            raw
        }
    )
    
    output$hover_info_raw <- renderUI({
        hover_raw <- input$plot_hover_raw

        select_data <- plotDataWW %>% 
            mutate(observation = observation/10^12) %>% filter(region == input$region, pathogen_type == input$pathogen)  %>%
            filter(date >= input$slider_dates[1] & date <= input$slider_dates[2]) 
        
        point <- nearPoints(select_data,
                            hover_raw, threshold = 8, maxpoints = 1, addDist = TRUE,
                            xvar = 'date', yvar = 'observation')
        if (nrow(point) == 0) return(NULL)
        
        left_px <- hover_raw$coords_css$x
        top_px <- hover_raw$coords_css$y
        
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.9); ",
                        "left:", left_px+2, "px; top:", top_px+2, "px;")
        
        # actual tooltip created as wellPanel
        wellPanel(
            style = style,
            p(HTML(paste0("<i>", point$date, "</i>", "<br/>",
                          "<b>", i18n$t("Gene copies"),"</b> (x10<sup>12</sup>): ", round(point$observation, 2), "<br/>",
                          "<i>(", i18n$t(as.character(point$quantification_flag)), ")</i>", "<br/>",
                          "<i>(",'Protocol: ',point$protocol ,")</i>")))
        )
    })
    
    # Plotting Rww+Re for other sources --------
    output$re_plots <- renderPlot(
        {
            re <- re_plotter(data = plotDataRe, source = input$data_type, canton = input$region, 
                                pathogen = input$pathogen, date_range = input$slider_dates, i18n = i18n)
            re
        }
    )
    
    output$hover_info_re <- renderUI({
        hover <- input$plot_hover_re

        select_data <- plotDataRe %>% 
          filter(region %in% catchment_to_cantons[[input$region]]) %>%
          filter(pathogen_type == input$pathogen) %>%
          filter(data_type %in% input$data_type) %>% 
          filter(date >= input$slider_dates[1] & date <= input$slider_dates[2])
                
        point <- nearPoints(select_data, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
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
    
    # plotting variants --------
    output$zh_plot <- renderPlot(
        {
            plot_Re(re_var %>% filter(source %in% input$variant_source), 'ZH', input$variant, 
                    'source', c('Wastewater (NGS)', 'Catchment cases'), c('Wastewater (NGS)', 'Catchment cases'))
        }
    )
    output$sg_plot <- renderPlot(
        {
            plot_Re(re_var %>% filter(source %in% input$variant_source), 'SG', 
                    input$variant, 'source', c('Wastewater (NGS)', 'Catchment cases'), c('Wastewater (NGS)', 'Catchment cases'))
        }
    )
    output$gr_plot <- renderPlot(
        {
            plot_Re(re_var %>% filter(source %in% input$variant_source), 'GR', 
                    input$variant, 'source', c('Wastewater (NGS)', 'Catchment cases'), c('Wastewater (NGS)', 'Catchment cases'))
        }
    )
    output$fr_plot <- renderPlot(
        {
            plot_Re(re_var %>% filter(source %in% input$variant_source), 'FR', 
                    input$variant, 'source', c('Wastewater (NGS)', 'Catchment cases'), c('Wastewater (NGS)', 'Catchment cases'))
        }
    )
    output$ti_plot <- renderPlot(
        {
            plot_Re(re_var %>% filter(source %in% input$variant_source), 'TI', 
                    input$variant, 'source', c('Wastewater (NGS)', 'Catchment cases'), c('Wastewater (NGS)', 'Catchment cases'))
        }
    )
    output$ge_plot <- renderPlot(
        {
            plot_Re(re_var %>% filter(source %in% input$variant_source), 'GE', 
                    input$variant, 'source', c('Wastewater (NGS)', 'Catchment cases'), c('Wastewater (NGS)', 'Catchment cases'))
        }
    )
    output$hover_info_variant_zh <- renderUI({
        hover <- input$plot_hover_variant_zh
        variant_data <- re_var %>% filter(source %in% input$variant_source) %>% filter(variant %in% input$variant) %>% filter(region == 'ZH')
        
        point <- nearPoints(variant_data, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
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
                          "Variant: ", point$variant, 
                          "<br/>",
                          "Source: ", point$source, 
                          "<br/>",
                          "<i>(", canton_to_catchment[[point$region]], ")</i>")))
        )
    })
    output$hover_info_variant_sg <- renderUI({
        hover <- input$plot_hover_variant_sg
        variant_data <- re_var %>% filter(source %in% input$variant_source) %>% filter(variant %in% input$variant) %>% filter(region == 'SG')
        
        point <- nearPoints(variant_data, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
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
                          "Variant: ", point$variant, 
                          "<br/>",
                          "Source: ", point$source, 
                          "<br/>",
                          "<i>(", canton_to_catchment[[point$region]], ")</i>")))
        )
    })
    output$hover_info_variant_gr <- renderUI({
        hover <- input$plot_hover_variant_gr
        variant_data <- re_var %>% filter(source %in% input$variant_source) %>% filter(variant %in% input$variant) %>% filter(region == 'GR')
        
        point <- nearPoints(variant_data, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
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
                          "Variant: ", point$variant, 
                          "<br/>",
                          "Source: ", point$source, 
                          "<br/>",
                          "<i>(", canton_to_catchment[[point$region]], ")</i>")))
        )
    })
    output$hover_info_variant_fr <- renderUI({
        hover <- input$plot_hover_variant_fr
        variant_data <- re_var %>% filter(source %in% input$variant_source) %>% filter(variant %in% input$variant) %>% filter(region == 'FR')
        
        point <- nearPoints(variant_data, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
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
                          "Variant: ", point$variant, 
                          "<br/>",
                          "Source: ", point$source, 
                          "<br/>",
                          "<i>(", canton_to_catchment[[point$region]], ")</i>")))
        )
    })
    output$hover_info_variant_ti <- renderUI({
        hover <- input$plot_hover_variant_ti
        variant_data <- re_var %>% filter(source %in% input$variant_source) %>% filter(variant %in% input$variant) %>% filter(region == 'TI')
        
        point <- nearPoints(variant_data, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
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
                          "Variant: ", point$variant, 
                          "<br/>",
                          "Source: ", point$source, 
                          "<br/>",
                          "<i>(", canton_to_catchment[[point$region]], ")</i>")))
        )
    })
    output$hover_info_variant_ge <- renderUI({
        hover <- input$plot_hover_variant_ge
        variant_data <- re_var %>% filter(source %in% input$variant_source) %>% filter(variant %in% input$variant) %>% filter(region == 'GE')
        
        point <- nearPoints(variant_data, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
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
                          "Variant: ", point$variant, 
                          "<br/>",
                          "Source: ", point$source, 
                          "<br/>",
                          "<i>(", canton_to_catchment[[point$region]], ")</i>")))
        )
    })
    
    # plotting all Rww ---------
    output$rww_plots <- renderPlot(
        {
            rww <- canton_plotter(source = 'Wastewater', canton = input$canton, 
                                  pathogen = input$pathogen,
                                  date_range = input$slider_dates_cantonal, i18n)
            title_p1 <- i18n$t("Estimated Wastewater R")
            title_p2 <- i18n$t("for different catchment areas")
            title_p3 <- ""
            if (nchar(title_p1)>25) {
                title_p1 <- "R"
                title_p3 <- gsub("R","",title_p1)
            }
            
            rww + guides(fill = "none", color = "none") +
              ggtitle(bquote(.(title_p1)['e']*.(title_p3)~.(title_p2)))
        }
    )
    
    output$hover_info_rww <- renderUI({
        hover <- input$plot_hover_rww
        select_data <- plotDataRe %>% filter(region %in% input$canton, pathogen_type == input$pathogen) %>%
            filter(data_type %in% 'Wastewater') %>% 
            filter(date >= input$slider_dates_cantonal[1] & date <= input$slider_dates_cantonal[2])
        
        point <- nearPoints(select_data, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
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
                          "<i>(", canton_to_catchment[[point$region]], ")</i>")))
        )
    })
    
    output$rcc_plots <- renderPlot(
        {
            rcc <- canton_plotter(canton = input$canton, source = 'Confirmed (Catchment)', pathogen = input$pathogen,
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
        hover <- input$plot_hover_rcc
        point <- nearPoints(plotDataRe %>% filter(region %in% input$canton) %>%
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
                          "<i>(", canton_to_catchment[[point$region]], ")</i>")))
        )
    })
    
    # text below plot for more info ---------
    output$link <- renderUI({
        
        link <- p(i18n$t("The raw measurements of SARS-CoV-2 in wastewater for this location are available "),
                  a(href = paste0("https://sensors-eawag.ch/sars/",tolower(canton_to_catchment[[input$region]]),".html"), i18n$t("here"), .noWS = "outside"),
                  ".",
                  .noWS = c("after-begin", "before-end"), style="margin-bottom:0;font-size: 95%;")
        
        if (input$region == "GE") {
            link <- p(i18n$t("The raw measurements of SARS-CoV-2 in wastewater for this location are available "),
                              a(href = paste0("https://sensors-eawag.ch/sars/geneve.html"), i18n$t("here"), .noWS = "outside"),
                              ".",
                              .noWS = c("after-begin", "before-end"), style="margin-bottom:0;font-size: 95%;")
        }
        
        exclude_lod <- p(HTML(paste0(strong('NB: '),i18n$t('Wastewater measurements from 02.2021 to 07.03.2021 for '), canton_to_catchment[[input$region]],
                                     i18n$t(' have been excluded due to multiple consecutive measurements falling below the limit of quantification and/or detection, which affects the reliability of the raw measurements and the wastewater R<sub>e</sub> estimates.'))),
                         style="margin-bottom:0;font-size: 95%;")
        
        laupen_2cantons <- p(strong('NB: '),i18n$t('The Laupen catchment area consists of municipalities in both Bern and Fribourg (13 communities from Bern and 12 from Fribourg).'), 
                             style="margin-bottom:0;font-size: 95%;")
        
        
        if (input$region == 'GR') HTML(paste(link, exclude_lod, sep = ""))
        else if (input$region == 'FR') HTML(paste(link, laupen_2cantons, sep = ""))
        else if (input$region == 'TI') HTML(paste(link, exclude_lod, sep = ""))
        else HTML(paste(link, sep = ""))
    })
    
    # download the plot ------
    output$downloadPlot <- downloadHandler(
        filename =  function() {
            paste0("Catchment_", canton_to_catchment[[input$region]], ".pdf")
        },
        # content is a function with argument file. content writes the plot to the device
        content = function(file) {
            case <- case_plotter(plotDataObs, input$region, input$pathogen, input$slider_dates, i18n)
            raw <- raw_plotter(plotDataWW, input$region, input$pathogen, input$slider_dates, i18n)
            if (input$region == "FR") {
                re <- re_plotter2(input$data_type, input$region, input$slider_dates, i18n) # call plotter 2: 2 cantons!
            } else {
                re <- re_plotter(plotDataRe, input$data_type, input$region, input$pathogen, input$slider_dates, i18n)
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
    
    # VARIANTS --------
    output$variant_disclaimer <- renderUI({
        HTML("<b>Note:</b><br>Variant R<sub>e</sub> is only calculated once the variant prevalence is above 2%. This is because for low variant prevalence, the estimates are volatile and not as reliable. Therefore, some variant options exist without corresponding estimates for wastewater and/or case data (such as BA.2.75).")
    })
}
