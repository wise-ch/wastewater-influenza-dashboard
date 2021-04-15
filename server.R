library(shiny)

#source("helper_code/reading_in.R")
source("helper_code/plotting_raw.R") # will call in reading_in
source("helper_code/plotting_re.R") # will call in processing
#source("helper_code/processing.R")


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
            all_raw_plots[[input$region]]
        }
    )
    output$re <- renderPlot(
        {
            all_re_plots[[input$region]]
        }
    )
    
}