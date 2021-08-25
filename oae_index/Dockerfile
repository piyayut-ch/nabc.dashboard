# Example shiny app docker file
# https://blog.sellorm.com/2021/04/25/shiny-app-in-docker/

# get shiny serveR and a version of R from the rocker project
FROM rocker/shiny-verse

# system libraries
# Try to only install system libraries you actually need
# Package Manager is a good resource to help discover system deps
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libcairo2-dev

# RUN apt-get update && \
#     apt-get upgrade -y && \
#     apt-get clean

# install R packages required 
# Change the packages list to suit your needs
RUN R -e 'install.packages(c(\
              "glue", "here", \
              "DT", \
              "extrafont", "hrbrthemes", "ggsci", "scales" \
            ), \
            repos="https://packagemanager.rstudio.com/cran/__linux__/focal/2021-07-01"\
          )'

# copy the app directory into the image
COPY . ./app

RUN Rscript -e 'extrafont::font_import("app/font", prompt = FALSE)'

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]