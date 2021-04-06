library(shiny)

source("helper_code/reading_in.R")

#### Define server logic ####
function(input, output) {
    
    # Save an object to the hist element of the output list (same as outputId).
    # Same name as in string in ui output (gets placed in the plot named "hist)
    # render functions: 
    # work with output function to place an R object as HTML on shiny webpage
    # render___ - type of object to build. Within it, we have a code block
    # also keeps track of reactivity - re-computes when input changes
    # use input values when you make your output. Access with $ and Id
    # this value changes as the input bar/slider/button changes. Reactive.
    output$raw <- renderPlot(
        {
            # from all the raw plots, it picks region
            # as per drop down menu
            #all_raw_plots[[input$region]]
        }
    )
    
}