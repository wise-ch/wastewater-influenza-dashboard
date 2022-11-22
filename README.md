# Scripts to calculate Re for influenza and a shiny app to display estimates

<!--- Link to published dashboard: [https://ibz-shiny.ethz.ch/wastewaterRe](https://ibz-shiny.ethz.ch/wastewaterRe) --->

## Repository overview
Wastewater data is provided by EAWAG and Canton Basel-Stadt, case data is provided by the Swiss FOPH. The raw data is expected to be in `data/raw_data`.
The pre-processing scripts in `R/helper_scripts` generate generate clean versions of these raw data files and save them to `data`. 

The Re estimation scripts `R/estimate_wastewater_re.R` and `R/estimate_confirmed_case_re.R` run automatically via Github actions when the clean versions of the data files are updated.
These scripts save Re estimates to `app/data`.

The shiny app can then be run to visualize the Re estimates.

## Docker image
The shinyapp can be reproduced using the docker image specified by `Dockerfile`.
```
docker build -t ww-shiny .
docker run --rm -p 8080:3838 ww-shiny
# Open app in browser: https://localhost:8080
```

The app can be published using the docker image as well:
```
docker build -t registry.ethz.ch/nadeaus/wastewater_re_shiny .
docker push registry.ethz.ch/nadeaus/wastewater_re_shiny
```

## Attribution

The code and app are based on prior work by Taru Singhal and Jana Huisman for SARS-CoV-2. The data on which this app is based is generated and provided by collaborators at EAWAG, the Canton Basel-Stadt, and the Swiss Federal Office of Public Health.

## Debugging the docker image
Build the image, then run a container locally in interactive mode:
```
docker run --rm -it -p 8080:3838 ww-shiny bash
# You should be inside the container now
/usr/bin/shiny-server  # Try lauching the app
# CTRL+C to stop the running app
cat /var/log/shiny-server/shiny-server-shiny-*.log  # Inspect the log to see what went wrong
exit  # Exit the container
```