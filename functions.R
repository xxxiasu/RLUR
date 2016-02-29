

################################################
## Get bounding box for a shapefile
## Returns Polygon in either wgs84 or input CRS
################################################
get.bbox <- function(x, wgs84, inCRS) {
  bb <- bbox(x)
  xy <- matrix(0, 4, 2)
  xy[1, 1] <- bb[1, 1]
  xy[1, 2] <- bb[2, 2]
  xy[2, 1] <- bb[1, 2]
  xy[2, 2] <- bb[2, 2]
  xy[3, 1] <- bb[1, 2]
  xy[3, 2] <- bb[2, 1]
  xy[4, 1] <- bb[1, 1]
  xy[4, 2] <- bb[2, 1]
  pb = Polygon(xy)
  ps = Polygons(list(pb),1)
  sps = SpatialPolygons(list(ps))
  prj <- paste0("+init=epsg:", inCRS)
  projection(sps) <- prj
  if (wgs84) {
    sps <- spTransform(sps, CRS("+proj=longlat +datum=WGS84"))
  }
  return (sps)
}

################################################
## Translate error code to message
################################################

error.message <- function(x) {
  if (x == 1)
    "Invalid EPSG code: Should be integer, see SpatialReference.org"
  else if (x == 2)
    "Expected a Point shapefile: Ensure you upload all parts at once - shp, dbf, prj, shx, sbn"
  else if (x == 3)
    "Expected a Polygon shapefile: Ensure you upload all parts at once - shp, dbf, prj, shx, sbn"
  else if (x == 4)
    "Expected a Line shapefile: Ensure you upload all parts at once - shp, dbf, prj, shx, sbn"
  else if (x == 5)
    "Expected a csv file"
  else if (x == 6)
    "Model not yet defined, please use the model builder"
  else if (x == 7)
    "No prediction area defined yet, please select on the map"
  else if (x == 8)
    "No Landcover (CORINE) data source specified"
  else if (x == 9)
    "No Road data source specified"
  else if (x == 10)
    "No Population data source specified"
  else if (x == 11)
    "No valid prediction points (no spatial overlap between defined prediction extent and input data)"
  else if (x == 12)
    "No valid prediction points (prediction area not specified)"
  else if (x == 13)
    "No terms in the model, please use the model builder"
  else if (x == 14)
    "Unknown (non-recognised LUR) variables in the model"
  else if (x == 15)
    "Error in calculating road nearest neighbours: Check data sources"
  else if (x == 16)
    "Error in calculating buffer-overlays for landcover: Check data sources"
  else if (x == 17)
    "Error in calculating buffer-overlays for road network: Check data sources"
  else if (x == 18)
    "Error in calculating buffer-overlays for population: Check data sources"
  else if (x == 19)
    "Output grid resolution not correctly defined: Should be an integer distance in input map units"
  else if (x == 20)
    "No prediction map exists to export"
  else if (x == 21)
    "Expecting an input point shapefile for receptors, rather than generating grid"
  else if (x == 22)
    "Unused"
  else if (x == 23)
    "Unused"
  else if (x == 24)
    "Unused"
  else if (x == 25)
    "Unused"
}

################################################
## Get R significance code for regression coefs
################################################
get.sig.star <- function(x) {
  if (x < 0.001) {
    return ("***")
  }
  else if (x < 0.01) {
    return ("**")
  }
  else if (x < 0.05) {
    return ("*")
  }
  else if (x < 0.1) {
    return (".")
  }
  else {
    return (" ")
  }
}

################################################
## Correlation plot
################################################

##http://personality-project.org/r/r.graphics.html
panel.cor <- function(x, y, digits=4, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * abs(r))
}
