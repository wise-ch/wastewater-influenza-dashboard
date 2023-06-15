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
          inputId = "measuring_periods",
          label = "Influenza season (select to compare):",
          choices = c("2021/22", "2022/23"),
          selected = c("2021/22", "2022/23")
        ),
        checkboxGroupInput(
          inputId = "data_type",
          label = "Data source (select to compare):",
          choices = c(
            "Virus load in wastewater" = "Wastewater",
            "Laboratory-confirmed cases" = "Confirmed cases"),
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
              height = "300px", width = "950px"
            ) %>%
              withSpinner(color = "#0dc5c1")
          ),
          htmlOutput("link"),
          downloadButton('download_plot', 'Download plots')
        ),
        p("*Note that catchment area refers to the intake area for each wastewater treatment plant. Laboratory-confirmed case numbers for the most recent 2-3 weeks are typically not yet complete due to reporting delays. Per-catchment case numbers are estimated based on postal code and a delineation for each catchment area, potentially resulting in non-integer case numbers. For more informaton on the specific catchments studied, see the ",
                   a(href = "https://www.eawag.ch/en/department/sww/projects/sars-cov2-in-wastewater/", "EAWAG project page for SARS-CoV-2 monitoring in wastewater", .noWS = "outside"), "."
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
        radioButtons(
          inputId = "measuring_period_all_catchments",
          label = "Influenza season:",
          choices = c("2021/22", "2022/23"),
          selected = "2021/22"
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
