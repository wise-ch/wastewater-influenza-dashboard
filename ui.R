library(shiny)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(shinyWidgets)
library(shiny.i18n)

source("helper_code/plot_maker.R")
i18n <- Translator$new(translation_json_path = "texts/translations.json")
i18n$set_translation_language("en-gb") # here you select the default translation to display

navbarPage("Covid-19: Wastewater Re",
           # a page with a navigation bar
           # HOME ####
           tabPanel(i18n$t("Catchments"),
                    shiny.i18n::usei18n(i18n),
                    
                    # Sidepanel - options + info  --------
                    sidebarLayout(
                        sidebarPanel(
                            selectInput(inputId = "region", 
                                        label = i18n$t("Select wastewater treatment plant:"),
                                        choices = c("Zurich" = "ZH",
                                                    #"Lausanne" = "VD",
                                                    "Altenrhein" = "SG", "Chur" = "GR",
                                                    "Laupen" = "FR", "Lugano" = "TI"
                                        ), # ask about these two cantonal catchments
                            ),
                            shinyjs::useShinyjs(),
                            checkboxGroupInput(inputId = "data_type",
                                               label = i18n$t("Data Source (select to compare):"),
                                               choices = c("Wastewater" = "Wastewater",
                                                           "Confirmed cases (in canton)" = "Confirmed (Canton)"),
                                               #"Deaths" = "Deaths",
                                               #"Hospitalized patients"= "Hospitalized patients"),
                                               selected = "Wastewater"),
                            checkboxGroupInput(inputId = 'catchment_selection',
                                               label = NULL,
                                               choices = c("Confirmed cases (in catchment area)" = "Confirmed (Catchment)")),
                            # if we would like deaths and hospitalised patients, comment out next chunk and
                            # include back in checkboxGroupInput
                            checkboxGroupInput(inputId = "data_type_disabled",
                                                        label = NULL,
                                                        choices = c("Deaths*" = "Deaths",
                                                                    "Hospitalized patients*"=
                                                                        "Hospitalized patients")),
                            uiOutput('death_hosp_info'),
                            
                            conditionalPanel(
                                condition = "input.region == 'GR'",
                                uiOutput('chur_catchment_disc')
                                
                            ),
                            
                            uiOutput('other_disclaimers'),
                            
                            selectInput(inputId = "lang", label = i18n$t("Language:"),
                                        choices = c("EN" = "en-gb",
                                                    #"Lausanne" = "VD",
                                                    "DE (in progress)" = "de-ch", 
                                                    "FR (in progress)" = "fr-ch", 
                                                    "IT (in progress)" = "it-ch"
                                        ), # ask about these two cantonal catchments
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
                                # idea 1: fix month range
                                # idea 2: can change both date ranges - current 
                                chooseSliderSkin("Flat", color = viridis(5)[4]),
                                #setSliderColor(viridis(5)[4], 1),
                                sliderInput("slider_dates", label = NULL, width = '950px',
                                            min = global_date_range[1], max = Sys.Date(), 
                                            value = c(global_date_range[1], Sys.Date())
                                ),
                                p(HTML(paste0('<em>', i18n$t('(The start and end date of the time interval to be displayed can be changed by moving the slider above.)'),'</em>')),
                                  style = 'margin-bottom:0;font-size: 90%;'),
                                htmlOutput("link"),
                                downloadButton('downloadPlot', 'Download results')
                                
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
                                                           #"Lausanne" = "VD",
                                                           "Altenrhein" = "SG", "Chur" = "GR",
                                                           "Laupen" = "FR", "Lugano" = "TI"),
                                               selected = c('ZH', 'SG', 'GR',
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
                                chooseSliderSkin("Flat", color = viridis(5)[4]),
                                #setSliderColor(viridis(5)[4], 1:2), # for some reason, this is not working?
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
           # About ---------
           tabPanel(i18n$t("About"),
                    fluidRow(column(
                        htmlOutput('about_page'),
                        width = 10),
                        hr()
                        
                    )
           )
)

