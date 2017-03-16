
#__RLUR: A dashboard for developing and applying Land Use Regression models for air pollution exposure estimation__

![s1](/www/s1.png)
![s2](/www/s2.png)
![s3](/www/s3.png)

*Developed on R.3.3.2 "Sincere Pumpkin Patch" and RStudio 1.0.136*

*See the help options in the software for more info on LUR and model development*

### __(1) TO DOWNLOAD__
-Click 'Clone or Download' then 'Download zip' button in Github (the green one above right) <br>
-Save the folder somewhere and unzip <br>
-Test data is in the 'testdata' folder <br>

### (2) GET R LIBRARIES
-In R. This needs doing only once<br>
-Run the following code to download/install the third-party packages needed<br>

```r
packages <- c("shinydashboard", "shiny", "car", "DT", "caret", "maptools", "rgdal", "raster", "sp", "rgeos", "leaflet", "shinyBS", "RColorBrewer")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
```

### (3) THERE ARE TWO WAYS TO RUN RLUR, BOTH NEED RSTUDIO

-(A) From local files:<br>
-Open either the downloaded 'server.r' or 'ui.r' file from (1) in RStudio<br>
-Use the drop-down next to 'run app' to make sure 'run external' is checked<br>
-Click 'Run App'<br>

-(B) Directly from GitHub:<br>
-With this method you do not need to download the files as in step (1)<br>
-Just run the following code in RStudio<br>

```r
require(shiny)
runGitHub("RLUR", "dwmorley") 
```

### (4) RUNNING ON A MAC

There is a dependency issue in the Mac version of the R PROJ4 package resulting in the error:
```r 
rgdal::checkCRSArgs: no proj_defs.dat in PROJ.4 shared files 
```
The solution is to recompile packages from source

A) Open a new terminal window to run the commands below (i.e. Not in R, a new command line prompt)<br>
B) Install homebrew package manager (http://brew.sh/)<br>
```r
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```
C) Re-compile OSGEO<br>
```r
brew tap osgeo/osgeo4mac
```
D) Update PROJ4<br>
```r
brew install proj
```
E) Restart any R sessions<br>


