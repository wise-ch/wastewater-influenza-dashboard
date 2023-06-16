# Dockerfile based on: https://www.r-bloggers.com/2021/06/running-shiny-server-in-docker/
FROM rocker/shiny-verse:4.2.0

# Install app's R dependencies
RUN install2.r --error --skipinstalled \
  dplyr \
  patchwork \
  RColorBrewer \
  readr \
  shiny \
  shinyBS \
  shinycssloaders \
  shinyjs \
  shinyWidgets \
  zoo \
  cowplot

# Copy app code into container
COPY ./app/ /srv/shiny-server/
COPY ./shiny-server.conf /etc/shiny-server/shiny-server.conf

USER shiny

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
