# Step 1: Use an official R base image
#FROM rocker/shiny:latest
#FROM rocker/r-base:latest
FROM rocker/r-ver:4.4.1

LABEL maintainer="Jani Miettinen <jani.miettinen@uef.fi>"

# Step 2: Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    sudo \
    pandoc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl3 \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    netcdf-bin \
    libharfbuzz-dev \
    libfribidi-dev \
    && rm -rf /var/lib/apt/lists/*

# Step 3: Install R packages needed for the app
RUN R -e "install.packages(c('arrow', 'ggpubr', 'data.table', 'markdown', 'shiny', 'raster', 'leaflet', 'here', 'hrbrthemes', 'ggplot2', 'dplyr', 'tidyr', 'DT', 'rhandsontable'), dependencies = TRUE, repos='https://cloud.r-project.org/')"


# Create the shiny user and group
RUN useradd -r -m shiny && \
    mkdir -p /srv/shiny-server/app && \
    chown -R shiny:shiny /srv/shiny-server/app

# Step 4: Copy the Shiny app into the container
WORKDIR /srv/shiny-server/app
COPY app /srv/shiny-server/app

# Step 5: Set permissions for the Shiny server
#RUN chown -R shiny:shiny /srv/shiny-server

# Step 6: Expose the port where the app will run
EXPOSE 3838

# Step 7: Start Shiny Server
#CMD ["/srv/shiny-server/app"]
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/app', host = '0.0.0.0', port=3838)"]