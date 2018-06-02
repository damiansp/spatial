#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/spatial/R/asdar')

library(rgdal)



# 1 Coordinate Reference Systems
epsg.news <- 'http://svn.osgeo.org/metacrs/proj/trunk/proj/NEWS'
proj4.news <- readLines(url(epsg.news))
lines <- grep('Release Notes|EPSG', proj4.news)
head(proj4.news[lines])

# 1.1 Using the EPSG list
EPSG <- make_EPSG()
head(EPSG)
class(EPSG)
EPSG[grep('^# ED50$', EPSG$note), ]

# 1.2 PROJ.4 CRS specifications
CRS('+init=epsg:4230')
ED50 <- CRS('+init=epsg:4230 +towgs84=-87,-96,-120,0,0,0,0')
ED50