# Dashboard for wastewater R<sub>e</sub> estimation

An R Shiny dashboard to visualise R<sub>e</sub> estimates from wastewater from six different wastewater treatment plants in Switzerland. These estimates are shown together with the catchment specific cases, SARS-CoV2 RNA data in the wastewater samples as well as R<sub>e</sub> estimated from other data sources (catchment and canton specific cases) along with their 95% confidence intervals. Catchment specific cases are included as the catchment area for a particular wastewater treatment plant differs from the cantonal area.

The six wastewater treatment plants we look at are: Zurich, Altenrhein, Chur, Laupen, Lugano and Lausanne. Currently, we have not included the Lausanne plant in the dashboard due to data quality issues.

<www.ibz-shiny.ethz.ch/...>

The methods used in estimating the R<sub>e</sub> estimation are described *[here](https://www.medrxiv.org/content/10.1101/2021.04.29.21255961v1.article-info)*.

## R Scripts

Core scripts:  
* `server.R`: Links the user interface with the instructions required to build and run the app.
* `ui.R`: Defines the user interface for the dashboard.<br>

Helper scripts:
* `reading_in.R`: Reads in the raw wastewater and case data for the six catchment areas from the respective *[EAWAG pages](https://sensors-eawag.ch/sars/overview.html)*. 
* `Rww_estimation.R`: Computes the R<sub>e</sub> for wastewater samples as well as catchment cases. It is run automatically using cron once a day (00:00 CEST). The script makes use of the wastewater R<sub>e</sub> pipeline available *[here](https://github.com/JSHuisman/wastewaterRe)*. These estimates are then written to a csv which is then used when the dashboard is launched.
* `plot_maker.R`: Reads in cantonal R<sub>e</sub> estimates and defines functions to plot cases, raw wastewater data and the estimated R<sub>e</sub>.

