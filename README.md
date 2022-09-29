# Dashboard for wastewater R<sub>e</sub> estimation

<!--- Link to published dashboard: [https://ibz-shiny.ethz.ch/wastewaterRe](https://ibz-shiny.ethz.ch/wastewaterRe) --->

An R Shiny dashboard to visualise R<sub>e</sub> estimates for viruses monitored in wastewater from six different wastewater treatment plants in Switzerland. These estimates are shown together with available data on clinical cases and raw measurements from wastewater. Note that wastewater catchment areas do not correspond one-to-one to specific cantons, though often we compare estimates based on wastewater to case data in the most relevant canton.

The six wastewater treatment plants we look at are: Zurich, Altenrhein, Chur, Laupen, Lugano and Lausanne. Currently, we have not included the Lausanne plant in the dashboard due to data quality issues.

The methods used to estimate the R<sub>e</sub> are described [here](https://www.medrxiv.org/content/10.1101/2021.04.29.21255961v1).

## Docker image
The app can be reproduced using the docker image specified by `Dockerfile`.

## R Scripts

Core scripts:  
* `app/server.R`: Links the user interface with the instructions required to build and run the app.
* `app/ui.R`: Defines the user interface for the dashboard.<br>

Helper scripts:
* `app/helper_code/reading_in/*.R`: Defines functions for reading in the raw wastewater and case data as well as R<sub>e</sub> estimates from various sources. SARS-CoV-2 data is from [EAWAG pages](https://sensors-eawag.ch/sars/overview.html). 
* `app/helper_code/plot_maker.R`: Calls the functions to read in the data. Further defines functions to plot cases, raw wastewater data and the estimated R<sub>e</sub>.

## Attribution

This app was created by Taru Singhal based on work by Jana Huisman. It was modified by Sarah Nadeau. The data on which this app is based is generated and provided by collaborators at EAWAG.
