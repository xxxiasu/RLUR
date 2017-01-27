Developed on R.3.3.2 "Sincere Pumpkin Patch" and RStudio 1.0.136

######################################################
## (1) TO DOWNLOAD
######################################################

## CLICK 'Clone or Download' then 'Download ZIP' BUTTON IN GITHUB (THE GREEN ONE ABOVE RIGHT)
## AND SAVE THE FOLDER SOMEWHERE AND UNZIP
## TEST DATA IS IN THE 'testData' FOLDER

######################################################
## (2) GET R LIBRARIES
######################################################

## IN RSTUDIO
## THIS ONLY NEEDS DOING ONCE
## RUN THE FOLLOWING CODE TO DOWNLOAD/INSTALL THE THIRD-PARTY PACKAGES NEEDED

packages <- c("shinydashboard", "shiny", "car", "DT", "caret", "maptools", "rgdal", "raster", "sp", "rgeos", "leaflet", "shinyBS", "RColorBrewer")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

######################################################
## (3) THERE ARE TWO WAYS TO RUN RLUR, BOTH NEED RSTUDIO
######################################################

## (A) FROM LOCAL FILES:
## OPEN EITHER THE DOWNLOADED 'SERVER.R' OR 'UI.R' FILE FROM (1) IN R STUDIO 
## USE THE DROP-DOWN NEXT TO 'Run App' TO MAKE SURE 'Run External' is checked
## CLICK 'Run App' 


## (B) DIRECTLY FROM GITHUB:
## WITH THIS METHOD YOU DO NOT NEED TO DOWNLOAD THE FILES AS IN STEP (1)
## JUST RUN THE FOLLOWING CODE IN RSTUDIO
## Make

require(shiny)
runGitHub( "RLUR", "dwmorley") 


######################################################
## (4) RUNNING ON A MAC
######################################################
 
## There is a dependency issue in the Mac version of the R PROJ4 package resulting in the error:
## rgdal::checkCRSArgs: no proj_defs.dat in PROJ.4 shared files
## The solution is to recompile packages from source
##
## A) Open a new terminal window to run the commands below (i.e. Not in R, a new command line prompt)
##
## B) Install homebrew package manager (http://brew.sh/)
## /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
##
## C) Re-compile OSGEO
## brew tap osgeo/osgeo4mac
##  
## D) Update PROJ4
## brew install proj
##
## E) Restart any R sessions

######################################################


