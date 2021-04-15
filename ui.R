library(shiny)

#### Define UI ####
navbarPage(HTML(paste0("Wastewater R",tags$sub("e"))),
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
                     #plotOutput("raw") - changing now ####
                     tabPanel("Plot",
                              # fluidRow( plotting ... )
                              plotOutput("raw"),
                              plotOutput("re"))
                 )
             )
    ),
    # About ####
    tabPanel("About"
             
    )
)

