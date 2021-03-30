library(shiny)

# Define server logic required to draw a histogram ----
function(input, output) {
    
    # Save an object to the hist element of the output list (same as outputId).
    # Same name as in string in ui output (gets placed in the plot named "hist)
    # render functions: 
    # work with output function to place an R object as HTML on shiny webpagr
    # render___ - type of object to build. Within it, we have a code block
    # also keeps track of reactivity - re-computes when input changes
    # use input values when you make your output. Access with $ and Id
    # this value changes as the input bar/slider/button changes. Reactive.
    output$hist <- renderPlot(
        {
            title <- "First histogram"
            hist(rnorm(input$num), main = title) # user chooses number of values
        }
    )
    
}