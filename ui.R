library(shiny)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(shinyWidgets)

source("helper_code/plot_maker.R")

navbarPage("Covid-19: Wastewater Re",
    # a page with a navigation bar
    # HOME ####
    tabPanel("Catchments",
             # Sidepanel - options + info  --------
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "region", label = "Select wastewater treatment plant:",
                                choices = c("Zurich" = "ZH",
                                            #"Lausanne" = "VD",
                                            "Altenrhein" = "SG", "Chur" = "GR",
                                            "Laupen" = "FR", "Lugano" = "TI"
                                            ), # ask about these two cantonal catchments
                                ),
                     shinyjs::useShinyjs(),
                     checkboxGroupInput(inputId = "data_type",
                                        label = "Data Source (select to compare):",
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
                     disabled(checkboxGroupInput(inputId = "data_type_disabled",
                                                 label = NULL,
                                                 choices = c("Deaths*" = "Deaths",
                                                             "Hospitalized patients*"=
                                                                 "Hospitalized patients"))),
                     p(HTML(paste0('*The R',tags$sub('e'),' for hospitalised patients and deaths are
                                   currently not displayed because the low case incidence results in
                                   large confidence intervals and low usefulness.')), style="font-size: 95%;"),
                     conditionalPanel(
                         condition = "input.region == 'GR'",
                         uiOutput('chur_catchment_disc')
                         
                     ),
                     p(
                         tags$ul(style="padding-left:10px;font-size: 95%;",
                            tags$li(HTML(paste0("The R",tags$sub('e'), " for wastewater is informed by
                                    infections in the catchment area, and will correspond best to
                                   the R",tags$sub('e')," based on confirmed cases
                                   from that area. All other R",tags$sub('e'),"
                                   traces show the cantonal results, so there may be some dissonance.
                                   For instance, canton Zurich is about 3.4x the size of the catchment area served by the
                                   Werdhölzli wastewater treatment plant."))),
                            tags$li('While Lausanne is also one of the catchment areas being monitored, 
                                           we have not included it in the dashboard due to data quality issues.') ) ),

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
                         p(HTML('<em>The start and end date of the time interval to be displayed can be changed by moving the slider above.</em>'),
                           style = 'margin-bottom:0;font-size: 90%;'),
                         htmlOutput("link"),
                         downloadButton('downloadPlot', 'Download results')

                     ) # fluid row
                 ) # main panel
             ) # Sidebar layout
    ), # Home panel
    # Switzerland  - rww comparison ----------
    tabPanel("Switzerland",
             sidebarLayout(
                 sidebarPanel(
                     checkboxGroupInput(inputId = "canton",
                                        label = "Canton (select to compare):",
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
                     p(HTML('<em>The start and end date of the time interval to be displayed can be changed by moving the slider above.</em>'),
                       style = 'margin-bottom:0;font-size: 90%;'),
                     p("The raw measurements of SARS-CoV-2 in wastewater and associated catchment cases are displayed on ",
                       a(href = "https://sensors-eawag.ch/sars/overview.html", "EAWAG's overview page", .noWS = "outside"),
                       " with links to individual plant measurements.",
                       .noWS = c("after-begin", "before-end"))
                 ) # fluid row
             )
        )

    ),
    # About ---------
    tabPanel("About",

             fluidRow(column(
                          h3("What is our aim?"),
                          p(HTML(paste0("We provide estimates of the effective reproductive number, R",tags$sub("e"),", based on longitudinal measurements of SARS-CoV-2 RNA in wastewater.
                          These estimates provide an independent account of COVID-19 transmission dynamics, complementing existing R",tags$sub("e"),
                          " estimates based on clinical data such as confirmed cases, hospitalisations and/or deaths. ","<br>",
                          "The purpose of this dashboard is to visualise the R",tags$sub("e"),"
                          estimates from wastewater for the wastewater treatment plants that are part of the AbwasSARS-CoV-2 project in Switzerland.
                          These estimates are shown together with existing R",tags$sub("e")," estimates from clinical data sources (for the corresponding cantons).")),
                            style="text-align:justify;color:black;padding:15px;border-radius:10px"),
                          h3("How do we do it?"),
                          p(HTML(paste0("The effective reproductive number, R",tags$sub("e"),", quantifies the expected number of people an infected individual will infect over time.
                          Since infected individuals excrete substantial amounts of SARS-CoV-2 RNA into the sewer system (via saliva, feces, and/or sputum),
                         wastewater samples reflect the number of people in a particular catchment area that are infected with COVID-19.
                            These longitudinal measurements of SARS-CoV-2 RNA in wastewater are then used to estimate R",tags$sub("e"),".","<br>", "As described in ")),
                            a(href = "https://www.medrxiv.org/content/10.1101/2021.04.29.21255961v1", "Huisman et al.", .noWS = "outside"),
                            HTML(paste0(", we use a pipeline that was previously developed to estimate R",tags$sub("e")," from clinical data sources (")),
                            a(href = "https://www.medrxiv.org/content/10.1101/2020.11.26.20239368v1.article-info", "Huisman, Scire et al.", .noWS = "outside"),
                            HTML(paste0(") and adapted it for use with wastewater measurements.
                            The main difference between both methods is how the underlying time series of infection events is inferred from the input observations:
                            clinical cases follow a delay distribution from infection to case confirmation, whereas wastewater measurements follow a
                            shedding load distribution from infection to viral shedding.")),
                            HTML(paste0("<br>",
                                        "Currently, these methods are applied to measurements of the ", strong("SARS-CoV-2 N1 gene"), "."
                                        )),
                            style="text-align:justify;color:black;padding:15px;border-radius:10px"),
                          h3("Who is involved?"),
                          p(HTML(paste0("This dashboard is developed by Taru Singhal, supervised by Jana Huisman and Tanja Stadler (")),
                            a(href = "https://bsse.ethz.ch/cevo", "Computational Evolution group", .noWS = "outside"),
                            HTML(paste0(", ETH Zurich).", "<br>",
                          "The underlying wastewater measurements are collected by teams at the Swiss Federal Institute of Aquatic Science and Technology (EAWAG) and EPFL,
                          supervised by Tim Julian, Christoph Ort, and Tamar Kohn.", "<br>",
                         "Funding for this project stems from the Swiss Federal Office of Public Health.")),
                            style="text-align:justify;color:black;padding:15px;border-radius:10px"),
                          h3("Links and further reading"),
                          p(HTML(paste0("The method to estimate R",tags$sub("e"), " from wastewater is described ")),
                            a(href = "https://www.medrxiv.org/content/10.1101/2021.04.29.21255961v1", "here", .noWS = "outside"),
                            " and the code is available ",
                            a(href = "https://github.com/JSHuisman/wastewaterRe", "here", .noWS = "outside"),
                            HTML(paste0(".", "<br>")),
                             HTML(paste0("The general R",tags$sub("e"), " estimation pipeline is described ")),
                            a(href = "https://www.medrxiv.org/content/10.1101/2020.11.26.20239368v1.article-info", "here", .noWS = "outside"),
                            " and the code is available ",
                            a(href = "https://github.com/covid-19-Re/shiny-dailyRe", "here", .noWS = "outside"), ".",
                            HTML("<br>"),
                            "The raw measurements of SARS-CoV-2 in wastewater are displayed on ",
                            a(href = "https://sensors-eawag.ch/sars/overview.html", "EAWAG's overview page", .noWS = "outside"),
                            " with links to the measurements at individual treatment plants.",
                            HTML("<br>"),
                            "For more information on the wastewater sampling process, please visit ",
                            a(href = "https://www.eawag.ch/en/department/sww/projects/sars-cov2-in-wastewater/", "EAWAG's SARS-CoV-2 project page", .noWS = "outside"),
                            ".",
                            .noWS = c("after-begin", "before-end"),
                            style="text-align:justify;color:black;padding:15px;border-radius:10px"),
                         h3("Contact us"),
                         p(HTML(paste0('For general inquiries, feedback or suggestions for improvements, please contact Taru Singhal (',
                         a(href='mailto:tsinghal@student.ethz.ch', 'tsinghal@student.ethz.ch'), ') or Jana Huisman (', 
                         a(href='mailto:jana.huisman@env.ethz.ch', 'jana.huisman@env.ethz.ch'), ').')), 
                           style="text-align:justify;color:black;padding:15px;border-radius:10px"),
                          width=10),


             hr()

        )
    )
)

