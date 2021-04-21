library(shiny)

#### Define UI ####
navbarPage("Covid-19: Wastewater Re",
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
                          h3("What is our aim?"), 
                          p(HTML(paste0("The effective reproductive number, R",tags$sub("e"),", quantifies the expected number of people an infected individual infects over time. 
                          Currently, such estimates for Covid-19 are based on clinical data such as confirmed cases, hospitalisations and/or deaths. 
                          Alhough, recently, progress has been made on using the SARS-CoV-2 RNA in wastewater to estimate the R",tags$sub("e"),". 
                          This is because even though SARS-CoV-2 is an airborne disease, substantial amounts are excreted (via saliva, feces, and/or sputum) 
                          into the sewershed. Wastewater sampling can therefore help quantify the extent to which people in a particular catchment area are infected with Covid-19.","<br>",
                                        "<br>", 
                            "The purpose of this dashboard is to visualise the wastewater R",tags$sub("e")," estimates for the different Swiss cantons 
                          which are collecting and analyzing wastewater samples for SARS-CoV-2 RNA. These estimates can then be compared to 
                          the existing R",tags$sub("e")," estimates from different data sources.")),
                            style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                          br(),
                          
                          h3("Who is involved?"), 
                          br(),
                          p(HTML(paste0("Jana Huissman et al. developed the pipeline to take the raw data of SARS-CoV-2 gene copies in wastewater and using the R",tags$sub("e")," estimation pipeline,
                          optimised it for wastewater R",tags$sub("e")," estimates. The methods used for the R",tags$sub("e"), " estimation pipeline are described ")),
                            a(href = "https://www.medrxiv.org/content/10.1101/2020.11.26.20239368v1.article-info", "here", .noWS = "outside"),
                            " and the code is available ", 
                            a(href = "https://github.com/covid-19-Re/shiny-dailyRe", "here", .noWS = "outside"), ".",
                            HTML("<br>"),HTML("<br>"),
                            "The raw SARS-CoV-2 in wastewater data are publicly available on the respective pages of the Swiss Federal Institute of Aquatic Science and Technology (EAWAG): ",
                            a(href = "https://www.eawag.ch/en/department/sww/projects/sars-cov2-in-wastewater/", "https://www.eawag.ch/en/department/sww/projects/sars-cov2-in-wastewater/", .noWS = "outside"),
                            ". EAWAG, along with labs from EPFL, are analyzing wastewater samples in cantons of Zurich, Lausanne and Ticino for the quantity of Covid-19 RNA present. 
                            Currently, the ", strong("SARS-CoV-2 N1 gene"), " is being isolated and analysed.",
                            .noWS = c("after-begin", "before-end"),
                            style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                          br(),

                          width=10),
             
             
             hr()
    
             
             
        )
    )
)

