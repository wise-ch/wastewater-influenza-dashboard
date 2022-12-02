library(shiny)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(shinyWidgets)

source("helper_code/make_plots.R")

navbarPage(
  title = "Wastewater Re",
  selected = "Single catchment",

  tabPanel(
    title = "Single catchment",

    sidebarLayout(

      # Sidepanel - options + info  --------
      sidebarPanel(
        selectInput(
          inputId = "wwtp",
          label = "Select wastewater treatment plant:",
          choices = c(
            "Zurich" = "ARA Werdhölzli Zurich",
            "Basel" = "ARA Basel",
            "Altenrhein" = "ARA Altenrhein",
            "Chur" = "ARA Chur",
            "Laupen" = "ARA Sensetal Laupen",
            "Lugano" = "IDA CDA Lugano",
            "Geneva" = "STEP Aire Geneva"
          ),
          selected = "ARA Werdhölzli Zurich"
        ),
        checkboxGroupInput(
          inputId = "data_type",
          label = "Data source (select to compare):",
          choices = c("Wastewater", "Confirmed cases"),
          selected = "Wastewater"
        ),
        width = 3
      ),

      # Home: main panel - all plotting and further info -------
      mainPanel(
        fluidRow(
          div(
            style = "position:relative",
            plotOutput("raw_plots",
              height = "250px", width = "950px"
            ) %>%
              withSpinner(color = "#0dc5c1")
          ),
          div(
            style = "position:relative",
            plotOutput("case_plots",
                       height = "250px", width = "950px"
            ) %>%
              withSpinner(color = "#0dc5c1")
          ),
          div(
            style = "position:relative",
            plotOutput("re_plots",
              height = "255px", width = "950px"
            ) %>%
              withSpinner(color = "#0dc5c1")
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
          downloadButton('download_plot', 'Download plots')
        ) # fluid row
      ) # main panel
    ) # Sidebar layout
  ), # Home panel

  # All-catchment comparison ----------
  tabPanel(
    "All catchments",

    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          inputId = "data_type_all_catchments",
          label = "Data source (select to compare):",
          choices = c("Wastewater", "Confirmed cases"),
          selected = "Wastewater"
        ),
        width = 3
      ),

      mainPanel(
        fluidRow(
          div(
            style = "position:relative",
            plotOutput("all_catchment_plots",
              height = "1050px", width = "980px"
            ) %>%
              withSpinner(color = "#0dc5c1")
          ),
          sliderInput("slider_dates_cantonal",
            label = NULL, width = "950px",
            min = as.Date("2021-02-01"), max = Sys.Date(),
            value = c(as.Date("2021-02-01"), Sys.Date())
          ),
          p(HTML(paste0("<em>", "(The start and end date of the time interval to be displayed can be changed by moving the slider above.)", "</em>")),
            style = "margin-bottom:0;font-size: 90%;"
          )
        ) # fluid row
      )  # main panel
    )  # Sidebar layout
  ),

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
  )
)
