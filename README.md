# Scripts to calculate Re for influenza and a shiny app to display estimates

<!--- Link to published dashboard: [https://ibz-shiny.ethz.ch/wastewaterRe](https://ibz-shiny.ethz.ch/wastewaterRe) --->

## Repository overview
Wastewater data is provided by EAWAG and Canton Basel-Stadt, case data is provided by the Swiss FOPH. The raw data is expected to be in `data/raw_data`.
The pre-processing scripts in `R/helper_scripts` generate generate clean versions of these raw data files and save them to `data`. 

The Re estimation scripts `R/estimate_wastewater_re.R` and `R/estimate_confirmed_case_re.R` run automatically via Github actions when new clean versions of the data files are pushed.
These scripts save Re estimates to `app/data`.

The shiny app can then be run to visualize the Re estimates. You can run the shiny app locally from RStudio by opening `app/server.R`, setting your working directory to the `app` directory, and clicking "Run App".

## Docker image
The shiny app can also be run locally using the docker image specified by `Dockerfile`. Once you have docker installed and the docker daemon running:
```
docker build -t ww-shiny .
docker run --rm -p 8080:3838 ww-shiny
# Open app in browser: https://localhost:8080
```

The app is actually deployed using this docker image. Another Github action workflow re-builds the image using the latest data in the repository every Monday, Wednesday, and Friday morning. This workflow can also be manually triggered from Github. The workflow pushes the re-built image to the Github package registry associated with this repository.

TODO: the server on which the shiny app is hosted should watch for new versions of the image.

To pull the production image and run it locally, follow the steps [here]( https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-container-registry) to log in to the Github package registry. Then run: 
```
docker pull ghcr.io/wise-ch/wastewater-influenza-dashboard:master
docker run --rm -p 8080:3838 ghcr.io/wise-ch/wastewater-influenza-dashboard:master
# Open app in browser: https://localhost:8080
```

## Server

The dashboard is currently hosted on an AWS server managed by Chaoran.

## Domain

The app is currently tied to a domain name owned by Chaoran: wise.covidhub.science.

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
