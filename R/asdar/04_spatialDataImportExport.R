#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/spatial/R/asdar')

library(DCluster)
library(maptools)
library(rgdal)
library(rgeos)
library(spacetime)
library(spdep)
data(wrld_simpl)

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
scot.dat <- read.table('data/scotland.dat', skip=1)
names(scot.dat) <- c(
  'District', 'Observed', 'Expected', 'PcAFF', 'Latitude', 'Longitude')
ogrInfo('data', 'scot')
scot.LL <- readOGR(dsn='data', layer='scot')
proj4string(scot.LL) <- CRS('+proj=longlat +ellps=WGS84')
plot(scot.LL)
sapply(slot(scot.LL, 'data'), class)
scot.LL$ID
scot.dat$District
ID.D <- match(scot.LL$ID, scot.dat$District)
scot.dat1 <- scot.dat[ID.D, ]
row.names(scot.dat1) <- row.names(scot.LL)
scot.LLa <- spCbind(scot.LL, scot.dat1)
all.equal(as.numeric(scot.LLa$ID), scot.LLa$District)
names(scot.LLa)

Obs <- scot.LLa$Observed
Exp <- scot.LLa$Expected
scot.LLa$SMR <- probmap(Obs, Exp)$relRisk / 100
scot.LLa$smth <- empbaysmooth(Obs, Exp)$smthrr
scot.BNG <- spTransform(scot.LLa, CRS('+init=epsg:27700'))
plot(scot.BNG)
drv <- 'ESRI Shapefile'
writeOGR(scot.BNG, dsn='data', layer='scotBNG', driver=drv)

dsn <- 'WFS:http://geohub.jrc.ec.europa.eu/effis/ows'
ogrListLayers(dsn) # not found :)
Fires <- readOGR(dsn, 'EFFIS:FiresAll')
names(Fires)

x <- c(-15, -15, 38, 38, -15)
y <- c(28, 62, 62, 28, 28)
crds <- cbind(x=x, y=y)
bb <- SpatialPolygons(list(Polygons(list(Polygon(coord=crds)), '1')))
proj4string(bb) <- CRS(proj4string(wrld_simpl))
slbb <- gIntersection(bb, as(wrld_simpl, 'SpatialLines'))
spl <- list('sp.lines', slbb, lwd=0.7, col='khaki4')

plot(slbb)
Fires$dt <- as.Date(as.character(Fires$FiereDate), format='%d-%m-%Y')
Fires0 <- Fires[-which(coordinates(Fires)[, 2] < 0), ]
Fires1 <- Fires0[order(Fires0$dt), ]
Fires2 <- STIDF(as(Fires1, 'SpatialPoints'), Fires1$dt, as(Fires1, 'data.frame'))
stplot(Fires2, number=3, sp.layout=spl, cex=0.5)
names(Fires1)[1] <- 'name'
GR.Fires <- Fires1[Fires1$Country == 'GR', ]
writeOGR(GR.Fires, 
         'data/EFFIS.gpx', 
         'waypoints', 
         driver='GPX', 
         dataset_options='GPX_USE_EXTENSIONS=YES')
GR <- readOGR('data/EFFIS.gpx', 'waypoints')
GR[1, c(5, 24:28)]


# 2.2 Other import/export functions
getinfo.shape('scot_BNG.shp')