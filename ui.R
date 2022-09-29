library(shiny)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(shinyWidgets)
library(shiny.i18n)

source("helper_code/plot_maker.R")
source('helper_code/variant_plots.R')
i18n <- Translator$new(translation_json_path = "texts/translations.json")
#i18n$set_translation_language("en-gb") # here you select the default translation to display

navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form navbar-right", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)
  navbar
}

navbarPageWithInputs("Wastewater Re",
           # a page with a navigation bar
           # HOME ####
           tabPanel(title = uiOutput("title_panel"),
                    shiny.i18n::usei18n(i18n),
                    
                    # Sidepanel - options + info  --------
                    sidebarLayout(
                        sidebarPanel(
                            selectInput(inputId = "region", 
                                        label = i18n$t("Select wastewater treatment plant:"),
                                        choices = c("Zurich" = "ZH",
                                                    "Basel" = "BS",
                                                    "Altenrhein" = "SG",
                                                    "Chur" = "GR",
                                                    "Laupen" = "FR",
                                                    "Lugano" = "TI",
                                                    "Geneva" = "GE"
                                        ),
                            ),
                            shinyjs::useShinyjs(),
                            uiOutput('pathogen'),
                            uiOutput('data_type'),
                            uiOutput('disabled_data_types'),
                            conditionalPanel(
                                condition = "input.region == 'GR'",
                                uiOutput('chur_catchment_disc')
                            ),
                            width = 3
                        ),
                        
                        # Home: main panel - all plotting and further info -------
                        mainPanel(
                            fluidRow(
                          
                              div(
                                    style = "position:relative",
                                    plotOutput("case_plots", height = "255px", width = "950px",
                                               hover = hoverOpts("plot_hover_case", delay = 10))%>%
                                        withSpinner(color="#0dc5c1"),
                                    uiOutput("hover_info_case")
                                ),
                                div(
                                    style = "position:relative",
                                    plotOutput("raw_plots", height = "250px", width = "950px",
                                               hover = hoverOpts("plot_hover_raw", delay = 10))%>%
                                        withSpinner(color="#0dc5c1"),
                                    uiOutput("hover_info_raw")
                                ),
                                div(
                                    style = "position:relative",
                                    plotOutput("re_plots", height = "255px", width = "950px",
                                               hover = hoverOpts("plot_hover_re", delay = 10))%>%
                                        withSpinner(color="#0dc5c1"),
                                    uiOutput("hover_info_re")
                                ),
                                # slider input for date range -----
                                shinyWidgets::setSliderColor(color = rep("darkgrey", 2), 1:2),
                                sliderInput("slider_dates", label = NULL, width = '950px',
                                            min = min(plotDataRe$date), max = Sys.Date(), 
                                            value = c(min(plotDataRe$date), Sys.Date())
                                ),
                                p(HTML(paste0('<em>', i18n$t('(The start and end date of the time interval to be displayed can be changed by moving the slider above.)'),'</em>')),
                                  style = 'margin-bottom:0;font-size: 90%;'),
                                htmlOutput("link"),
                                # downloadButton('downloadPlot', i18n$t('Download results'))  # TODO: re-implement download function
                                
                            ) # fluid row
                        ) # main panel
                    ) # Sidebar layout
           ), # Home panel
           # Switzerland  - rww comparison ----------
           tabPanel(i18n$t("Switzerland"),
                    sidebarLayout(
                        sidebarPanel(
                            checkboxGroupInput(inputId = "canton",
                                               label = i18n$t("Catchment (select to compare):"),
                                               choices = c("Zurich" = "ZH",
                                                           "Geneva" = "GE",
                                                           "Altenrhein" = "SG",
                                                           "Chur" = "GR",
                                                           "Laupen" = "FR",
                                                           "Lugano" = "TI"),
                                               selected = c('ZH', 'GE',
                                                            'SG', 'GR',
                                                            'FR', 'TI')),
                            width = 3
                        ),
                        mainPanel(
                            fluidRow(
                                div(
                                    style = "position:relative",
                                    plotOutput("rww_plots", height = "350px", width = "980px",
                                               hover = hoverOpts("plot_hover_rww", delay = 10))%>%
                                        withSpinner(color="#0dc5c1"),
                                    uiOutput("hover_info_rww")
                                ),
                                div(
                                    style = "position:relative",
                                    plotOutput("rcc_plots", height = "350px", width = "980px",
                                               hover = hoverOpts("plot_hover_rcc", delay = 10))%>%
                                        withSpinner(color="#0dc5c1"),
                                    uiOutput("hover_info_rcc")
                                ),
                                #shinyWidgets::setSliderColor(color = "#5dc863FF", 1:2), # for some reason, this is not working?
                                #shinyWidgets::chooseSliderSkin("Flat", color = "#5dc863FF"),
                                sliderInput("slider_dates_cantonal", label = NULL, width = '950px',
                                            min = as.Date('2021-02-01'), max = Sys.Date(),
                                            value = c(as.Date('2021-02-01'), Sys.Date())
                                ),
                                p(HTML(paste0('<em>', i18n$t('(The start and end date of the time interval to be displayed can be changed by moving the slider above.)'),'</em>')),
                                  style = 'margin-bottom:0;font-size: 90%;'),
                                p(i18n$t("The raw measurements of SARS-CoV-2 in wastewater and associated catchment cases are displayed on "),
                                  a(href = "https://sensors-eawag.ch/sars/overview.html", i18n$t("EAWAG's overview page"), .noWS = "outside"),
                                  i18n$t(" with links to individual plant measurements."),
                                  .noWS = c("after-begin", "before-end"))
                            ) # fluid row
                        )
                    )
                    
           ),
           # Variants -----------
           tabPanel('Variants',
                    sidebarLayout(
                      sidebarPanel(
                        checkboxGroupInput(inputId = "variant", 
                                    label = i18n$t("Select variant:"),
                                    choices = c("Alpha (B.1.1.7)" = "B.1.1.7",
                                                "Delta (B.1.617.2)" = "B.1.617.2", 
                                                'Omicron (BA.1)' = 'BA.1',
                                                'Omicron (BA.2)' = 'BA.2',
                                                'Omicron (BA.2.75)' = 'BA.2.75',
                                                'Omicron (BA.4)' = 'BA.4',
                                                'Omicron (BA.5)' = 'BA.5'),
                                    selected = 'BA.2'
                        ),
                        checkboxGroupInput(inputId = "variant_source", 
                                           label = i18n$t("Select data source:"),
                                           choices = c("Wastewater (NGS)" = "Wastewater (NGS)",
                                                       "Catchment cases" = "Catchment cases"),
                                           selected = 'Wastewater (NGS)'
                        ),
                        htmlOutput('variant_disclaimer'),
                        width = 3
                      ),
                      
                      # Home: main panel - all plotting and further info -------
                      mainPanel(
                        fluidRow(
                                 div(
                                   style = "position:relative",
                                   plotOutput("zh_plot", height = "255px", width = "950px",
                                              hover = hoverOpts("plot_hover_variant_zh", delay = 10))%>%
                                     withSpinner(color="#0dc5c1"),
                                   uiOutput("hover_info_variant_zh")
                                 ),
                                 div(
                                   style = "position:relative",
                                   plotOutput("sg_plot", height = "255px", width = "950px",
                                              hover = hoverOpts("plot_hover_variant_sg", delay = 10))%>%
                                     withSpinner(color="#0dc5c1"),
                                   uiOutput("hover_info_variant_sg")
                                 ),
                                 div(
                                   style = "position:relative",
                                   plotOutput("gr_plot", height = "255px", width = "950px",
                                              hover = hoverOpts("plot_hover_variant_gr", delay = 10))%>%
                                     withSpinner(color="#0dc5c1"),
                                   uiOutput("hover_info_variant_gr")
                                 ),
                                 div(
                                   style = "position:relative",
                                   plotOutput("fr_plot", height = "255px", width = "950px",
                                              hover = hoverOpts("plot_hover_variant_fr", delay = 10))%>%
                                     withSpinner(color="#0dc5c1"),
                                   uiOutput("hover_info_variant_fr")
                                 ),
                                 div(
                                   style = "position:relative",
                                   plotOutput("ti_plot", height = "255px", width = "950px",
                                              hover = hoverOpts("plot_hover_variant_ti", delay = 10))%>%
                                     withSpinner(color="#0dc5c1"),
                                   uiOutput("hover_info_variant_ti")
                                 ),
                                 div(
                                   style = "position:relative",
                                   plotOutput("ge_plot", height = "255px", width = "950px",
                                              hover = hoverOpts("plot_hover_variant_ge", delay = 10))%>%
                                     withSpinner(color="#0dc5c1"),
                                   uiOutput("hover_info_variant_ge")
                                 )
                        ) # fluid row
                      ) # main panel
                    ) # Sidebar layout
           ),
           # About ---------
           tabPanel(i18n$t("About"),
                    fluidRow(column(
                      htmlOutput('about_page'),
                      width = 10),
                      hr()
                      
                    )
           ),
           inputs = selectInput(inputId = "lang", label = NULL,
                                choices = c("EN" = "en-gb",
                                            "DE" = "de-ch", 
                                            "FR" = "fr-ch", 
                                            "IT" = "it-ch"
                                ), width = '80px')
)


