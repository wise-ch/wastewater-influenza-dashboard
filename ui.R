library(shiny)


# fluidPage(
#     # Title
#     titlePanel("Wastewater Re Estimations"),
#     
#     
#     
#     # creating an input. A dozen input functions available.
#     #sliderInput(inputId = "num", #  Assign a name to the input. Used to identify input
#     #            label = "Choose a number", # what the user will see
#     #            value = 25, # default value
#     #            min = 1, 
#     #            max = 100), # value, min, max for sliders. Different for each inputs.
#     
#     plotOutput(outputId = "raw_zh") # name to use for output, have a different one for each output
#     
#     
# )

#### Define UI ####
navbarPage("Re Estimations",
    # a page with a navigation bar
    tabPanel("Home",
             # in the sidebar dropdown, you can pick region
             sidebarLayout(
                 sidebarPanel(
                     selectInput(inputId = "region", label = "Region:",
                                choices = c("Zurich" = "zh", "Plant2" = "pl"))
                 ),
                 # in the main panel, you will see the raw plot
                 # Re plot also needs to be added.
                 mainPanel(
                     plotOutput("raw")
                 )
             )
    ),
    tabPanel("About"
             
    )
)