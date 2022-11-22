# Dockerfile based on: https://www.r-bloggers.com/2021/06/running-shiny-server-in-docker/
FROM rocker/shiny-verse:4.2.0

# Install app's R dependencies
RUN install2.r --error --skipinstalled \
  dplyr \
  RColorBrewer \
  readr \
  shiny \
  shinyBS \
  shinycssloaders \
  shinyjs \
  shinyWidgets

# Copy app code into container
COPY ./app/ /srv/shiny-server/
RUN echo "preserve_logs true;" >> /etc/shiny-server/shiny-server.conf  # will save logs to /var/log/shiny-server inside container

USER shiny

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]