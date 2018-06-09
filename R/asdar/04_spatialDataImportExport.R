#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/spatial/R/asdar')

library(maptools)
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

# 1.3 Projection and transformation
IJ.east <- as(char2dms("4d31'00\"E"), 'numeric')
IJ.north <- as.numeric(char2dms('52d28\'00"N'))
IJ.ED50 <- SpatialPoints(cbind(x=IJ.east, y=IJ.north), proj4string=ED50)
res <- spTransform(IJ.ED50, CRS('+proj=longlat +datum=WGS84'))
x <- as.character(dd2dms(coordinates(res)[1]))
y <- as.character(dd2dms(coordinates(res)[2], T))
cat(x, y, '\n')
spDistsN1(coordinates(IJ.ED50), coordinates(res), longlat=T) * 1000
gzAzimuth(coordinates(IJ.ED50), coordinates(res))

proj4string(IJ.ED50) <- CRS('+init=epsg:4230')
res <- spTransform(IJ.ED50, CRS('+proj=longlat +datum=WGS84'))
spDistsN1(coordinates(IJ.ED50), coordinates(res), longlat=T) * 1000 
gzAzimuth(coordinates(IJ.ED50), coordinates(res))

EPSG[grep('Atlas', EPSG$note), 1:2]
CRS('+init=epsg:2163')
proj <- projInfo('proj')
proj[proj$name == 'laea', ]
ellps <- projInfo('ellps')
ellps[grep('a=6370997', ellps$major), ]

# 1.4 Degrees, minutes, and seconds (DMS)
getSlots('DMS')
as.numeric(char2dms('4d31\'00"E'))



# 2 Vector File Formats
# 2.1 Using OGR drivers in rgdal
head(ogrDrivers())