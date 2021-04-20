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
                                choices = c("Zurich" = "ZH", "Lausanne" = "VD"))
                 ),
                 # in the main panel, you will see the raw plot
                 # Re plot also needs to be added.
                 # Home: main panel ####
                 mainPanel(
                     #plotOutput("raw") - changing now ####
                     tabPanel("Plot",
                              # fluidRow( plotting ... )
                              #plotOutput("raw"),
                              plotOutput("plots"))
                 )
             )
    ),
    # About ####
    tabPanel("About",
             fluidRow(column(
                          
                          br(),
                          p(HTML(paste0("The effective reproductive number, R",tags$sub("e"),", quantifies the expected number of people an infected individual infects over time. 
                          Currently, such estimates for Covid-19 are based on clinical data such as confirmed cases, hospitalisations and/or deaths. 
                          Alhough, recently, progress has been made on using the Covid-19 RNA in wastewater to estimate the R",tags$sub("e"),". 
                          This is because even though Covid-19 is an airborne disease, substantial amounts are excreted (via saliva, feces, and/or sputum) 
                          into the sewershed. Wastewater sampling can therefore help quantify the extent to which people in a particular catchment area are infected with Covid-19. 
                          Jana Huissman developed the pipeline to take the raw data of gene copies in wastewater and using the R",tags$sub("e")," estimation pipeline (link below),
                          optimise it for wastewater R",tags$sub("e")," estimates.")),
                            style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                          br(),
                          
                          p(HTML(paste0("The purpose of this dashboard is to visualise the R",tags$sub("e")," estimates for the different cantons 
                          which are collecting and analyzing wastewater samples for Covid-19 RNA. These estimates can then be compared to 
                          the existing R",tags$sub("e")," estimates from different data sources.")),
                            style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                          br(),

                          p("The raw data used in this process are publicly available on the respective pages of the Swiss Federal Institute of Aquatic Science and Technology (EAWAG): ",
                            a(href = "https://www.eawag.ch/en/department/sww/projects/sars-cov2-in-wastewater/", "https://www.eawag.ch/en/department/sww/projects/sars-cov2-in-wastewater/", .noWS = "outside"),
                            ". EAWAG, along with labs from EPFL, are analyzing wastewater samples in cantons of Zurich, Lausanne and Ticino for the quantity of Covid-19 RNA present.",.noWS = c("after-begin", "before-end"),
                          style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                          br(),
                          
                          p(HTML(paste0("The code for the R",tags$sub("e")," estimation pipeline is available on: ")),
                            a(href = "https://github.com/covid-19-Re/shiny-dailyRe", "https://github.com/covid-19-Re/shiny-dailyRe", .noWS = "outside"),
                            ".",
                            style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                          
                          br(),
                          
                          width=10),
             
             
             hr()
    
             
             
        )
    )
)

