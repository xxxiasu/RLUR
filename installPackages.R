#######################################
## ENSURE REQUIRED PACKAGES ARE INSTALLED
#######################################

get.packages <- function() {
  packages <- c("shiny","arm", "car", "DT", "caret", "maptools", "rgdal", "raster", "sp", "rgeos", "leaflet", "shinyBS", "RColorBrewer")
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
  }  
}



