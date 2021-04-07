library(shiny)

#### Define UI ####
navbarPage("Re Estimations",
    # a page with a navigation bar
    # HOME ####
    tabPanel("Home",
             # in the sidebar dropdown, you can pick region
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "region", label = "Region:",
                                choices = c("Zurich" = "zh", "Plant2" = "pl"))
                 ),
                 # in the main panel, you will see the raw plot
                 # Re plot also needs to be added.
                 # Home: main panel ####
                 mainPanel(
                     plotOutput("raw")
                 )
             )
    ),
    # About ####
    tabPanel("About"
             
    )
)

