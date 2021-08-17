library(shiny)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(shinyWidgets)
library(shiny.i18n)

source("helper_code/plot_maker.R")
i18n <- Translator$new(translation_json_path = "texts/translations.json")
#i18n$set_translation_language("en-gb") # here you select the default translation to display

navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form navbar-right", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form)
  navbar
}

navbarPageWithInputs("Covid-19: Wastewater Re",
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
                                                    #"Lausanne" = "VD",
                                                    "Altenrhein" = "SG", "Chur" = "GR",
                                                    "Laupen" = "FR", "Lugano" = "TI"
                                        ), 
                            ),
                            shinyjs::useShinyjs(),
                            uiOutput('data_type'),
                            uiOutput('catchment'),
                            uiOutput('disabled'),
                            uiOutput('death_hosp_info'),
                            
                            conditionalPanel(
                                condition = "input.region == 'GR'",
                                uiOutput('chur_catchment_disc')
                                
                            ),
                            
                            uiOutput('other_disclaimers'),

                            width = 3
                        ),
                        
                        # Home: main panel - all plotting and further info -------
                        mainPanel(
                            fluidRow(
                              #p("Disclaimer: There is currently a bug in the slider we are trying to fix.",style = 'margin-bottom:0;font-size: 90%;color: red'),
                              #br(),  
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
                                shinyWidgets::setSliderColor(color = rep("#5dc863FF", 2), 1:2), # for some reason, this is not working?
                                #shinyWidgets::chooseSliderSkin("Flat", color = "#5dc863FF"),
                                sliderInput("slider_dates", label = NULL, width = '950px',
                                            min = global_date_range[1], max = Sys.Date(), 
                                            value = c(global_date_range[1], Sys.Date())
                                ),
                                p(HTML(paste0('<em>', i18n$t('(The start and end date of the time interval to be displayed can be changed by moving the slider above.)'),'</em>')),
                                  style = 'margin-bottom:0;font-size: 90%;'),
                                htmlOutput("link"),
                                downloadButton('downloadPlot', i18n$t('Download results'))
                                
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
                                               selected = c('ZH', #'VD',
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


