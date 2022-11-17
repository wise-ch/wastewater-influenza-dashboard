library(shiny)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(shinyWidgets)

source("helper_code/make_plots.R")

navbarPageWithInputs <- function(..., inputs) {
  navbar <- navbarPage(...)
  form <- tags$form(class = "navbar-form navbar-right", inputs)
  navbar[[3]][[1]]$children[[1]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]], form
  )
  navbar
}

navbarPageWithInputs("Wastewater Re",
  # a page with a navigation bar
  # HOME ####
  tabPanel(
    title = uiOutput("title_panel"),

    # Sidepanel - options + info  --------
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = "wwtp",
          label = "Select wastewater treatment plant:",
          choices = c(
            "Zurich" = "ARA Werhölzli Zurich",
            "Basel" = "ARA Basel",
            "Altenrhein" = "ARA Altenrhein",
            "Chur" = "ARA Chur",
            "Laupen" = "ARA Sensetal Laupen",
            "Lugano" = "IDA CDA Lugano",
            "Geneva" = "STEP Aire Geneva"
          ),
        ),
        shinyjs::useShinyjs(),
        uiOutput("data_type"),
        uiOutput("disabled_data_types"),
        width = 3
      ),

      # Home: main panel - all plotting and further info -------
      mainPanel(
        fluidRow(
          div(
            style = "position:relative",
            plotOutput("raw_plots",
              height = "250px", width = "950px",
              hover = hoverOpts("plot_hover_raw", delay = 10)
            ) %>%
              withSpinner(color = "#0dc5c1"),
            uiOutput("hover_info_raw")
          ),
          div(
            style = "position:relative",
            plotOutput("re_plots",
              height = "255px", width = "950px",
              hover = hoverOpts("plot_hover_re", delay = 10)
            ) %>%
              withSpinner(color = "#0dc5c1"),
            uiOutput("hover_info_re")
          ),
          # slider input for date range -----
          shinyWidgets::setSliderColor(color = rep("darkgrey", 2), 1:2),
          sliderInput("slider_dates",
            label = NULL, width = "950px",
            min = min(re_to_plot$date), max = Sys.Date(),
            value = c(min(re_to_plot$date), Sys.Date())
          ),
          p(HTML(paste0("<em>", "(The start and end date of the time interval to be displayed can be changed by moving the slider above.)"), "</em>"),
            style = "margin-bottom:0;font-size: 90%;"
          ),
          htmlOutput("link"),
          # downloadButton('downloadPlot', i18n$t('Download results'))  # TODO: re-implement download function
        ) # fluid row
      ) # main panel
    ) # Sidebar layout
  ), # Home panel
  
  # # Switzerland  - rww comparison ----------
  # tabPanel(
  #   i18n$t("Switzerland"),
  #   sidebarLayout(
  #     sidebarPanel(
  #       selectInput(
  #         inputId = "pathogen_switzerland",
  #         label = i18n$t("Select pathogen:"),
  #         choices = c(
  #           "SARS-CoV-2" = "COVID",
  #           "Influenza A Virus" = "IAV",
  #           "Influenza B Virus" = "IBV"
  #         ),
  #         selected = "COVID"
  #       ),
  #       shinyjs::useShinyjs(),
  #       uiOutput("region_switzerland"),
  #       uiOutput("region_switzerland_disabled"),
  #       width = 3
  #     ),
  #     mainPanel(
  #       fluidRow(
  #         div(
  #           style = "position:relative",
  #           plotOutput("rww_plots",
  #             height = "350px", width = "980px",
  #             hover = hoverOpts("plot_hover_rww", delay = 10)
  #           ) %>%
  #             withSpinner(color = "#0dc5c1"),
  #           uiOutput("hover_info_rww")
  #         ),
  #         div(
  #           style = "position:relative",
  #           plotOutput("rcc_plots",
  #             height = "350px", width = "980px",
  #             hover = hoverOpts("plot_hover_rcc", delay = 10)
  #           ) %>%
  #             withSpinner(color = "#0dc5c1"),
  #           uiOutput("hover_info_rcc")
  #         ),
  #         # shinyWidgets::setSliderColor(color = "#5dc863FF", 1:2), # for some reason, this is not working?
  #         # shinyWidgets::chooseSliderSkin("Flat", color = "#5dc863FF"),
  #         sliderInput("slider_dates_cantonal",
  #           label = NULL, width = "950px",
  #           min = as.Date("2021-02-01"), max = Sys.Date(),
  #           value = c(as.Date("2021-02-01"), Sys.Date())
  #         ),
  #         p(HTML(paste0("<em>", i18n$t("(The start and end date of the time interval to be displayed can be changed by moving the slider above.)"), "</em>")),
  #           style = "margin-bottom:0;font-size: 90%;"
  #         ),
  #         p(i18n$t("The raw measurements of SARS-CoV-2 in wastewater and associated catchment cases are displayed on "),
  #           a(href = "https://sensors-eawag.ch/sars/overview.html", i18n$t("EAWAG's overview page"), .noWS = "outside"),
  #           i18n$t(" with links to individual plant measurements."),
  #           .noWS = c("after-begin", "before-end")
  #         )
  #       ) # fluid row
  #     )
  #   )
  # ),
  
  # About ---------
  tabPanel(
    "About",
    fluidRow(
      column(
        htmlOutput("about_page"),
        width = 10
      ),
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
