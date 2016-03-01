## TO RUN VIA GITHUB
## NEEDS RSTUDIO INSTALLED

## IN R STUDIO:

## GET PACKAGES (ONLY NEEDS DOING ONCE)
packages <- c("shiny", "car", "DT", "caret", "maptools", "rgdal", "raster", "sp", "rgeos", "leaflet", "shinyBS", "RColorBrewer")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}  

## RUN THE APPLICATION
require(shiny)
runGitHub( "RLUR", "dwmorley") 