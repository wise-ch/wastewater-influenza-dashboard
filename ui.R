library(shiny)

# Define UI for application that draws a histogram ----
fluidPage(
    "hello world",
    # creating an input. A dozen input functions.
    sliderInput(inputId = "num", #  Assign a name to the input. Used to identify input
                label = "Choose a number", # what the user will see
                value = 25, # default value
                min = 1, 
                max = 100), # value, min, max for sliders. Different for each inputs.
    
    plotOutput(outputId = "hist") # name to use for output, have a different one for each output
    
    
)
