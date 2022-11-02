# Use the rocker image as a base image
FROM rocker/geospatial

RUN apt-get update -qq && \
    apt-get install -qq libxt-dev r-cran-cairo
    
COPY . ./home/rstudio/app/

# Change the user settings
RUN mkdir -p ./home/rstudio/.config/rstudio/
RUN cp ./home/rstudio/app/rstudio-prefs.json ./home/rstudio/.config/rstudio/
