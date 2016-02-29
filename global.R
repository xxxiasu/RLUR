require(shiny) 
require(RColorBrewer)

##################################################
## VARIABLE LISTS ACCORDING TO ESCAPE PROJECT
##################################################

corine.vars <-
  c("HDRES", "LDRES", "INDUSTRY", "PORT", "URBGREEN", "NATURAL")

roads.vars.length.maj <-
  c("TRAFMAJORLOAD", "HEAVYTRAFMAJORLOAD", "MAJORROADLENGTH")

roads.vars.length.all <-
  c("HEAVYTRAFLOAD", "ROADLENGTH", "TRAFLOAD")

roads.vars.nn.maj <-
  c(
    "HEAVYTRAFMAJOR", "DISTINVMAJOR1", "DISTINVMAJOR2", "INTMAJORINVDIST", "INTMAJORINVDIST2", "TRAFMAJOR"
  )

roads.vars.nn.all <-
  c(
    "TRAFNEAR", "DISTINVNEAR1", "DISTINVNEAR2", "INTINVDIST", "INTINVDIST2", "HEAVYTRAFNEAR"
  )

popn.vars <- c("POP", "HHOLD")


##################################################
## IN DATA
##################################################

filename.monitor <- reactiveValues() #filenames
filename.roads <- reactiveValues()
filename.corine <- reactiveValues()
filename.popn <- reactiveValues()
filename.exist <- reactiveValues()
filename.recept <- reactiveValues()

inFile <- reactiveValues()
in.data <- reactiveValues()
dat <- reactiveValues() 
epsg <- reactiveValues() #epsg code

gis <- reactiveValues() #T/F if variables created


##################################################
## OUT DATA
##################################################
pred.grid <- NULL # points to make predictions at
r <- NULL # prediction raster or shapefile


##################################################
## MODELLING
##################################################
lur.model <- reactiveValues()
lur.loocv <- NULL

buffers.1 <- c(5000, 1000, 500, 300, 100)
buffers.2 <- c(1000, 500, 300, 100, 50, 25)


##################################################
## LEAFLET
##################################################
ext <- reactiveValues() # Extent of predictor data
ext$min.long <- NULL
ext$min.lat <- NULL
ext$max.long <- NULL
ext$max.lat <- NULL

x <- 0 # Counter to help clear start marker
vertices <- reactiveValues() # Polygon verticies
vertices$xy <- matrix(0, 0, 2)

pal <- rev(brewer.pal(11, "RdYlGn"))

