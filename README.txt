## TO RUN VIA GITHUB
## NEEDS RSTUDIO INSTALLED

## IN R STUDIO:

## GET PACKAGES (ONLY NEEDS DOING ONCE)
packages <- c("shiny","arm", "car", "DT", "caret", "maptools", "rgdal", "raster", "sp", "rgeos", "leaflet", "shinyBS", "RColorBrewer")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}  

## RUN THE APP
require(shiny)
runGitHub( "RLUR", "dwmorley") 