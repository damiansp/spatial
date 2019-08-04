#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/spatial/R/asdar')

library(DCluster)
library(gstat)
library(maptools)
library(rgdal)
library(rgeos)
library(spacetime)
library(spdep)

data(meuse)
data(meuse.grid)
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
IJ.dms.E <- '4d31\'00"E'
IJ.dms.N <- '52d28\'00"N'
IJ.east <- char2dms(IJ.dms.E)
IJ.north <- char2dms(IJ.dms.N)
IJ.east
IJ.north
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
ogrListLayers(dsn) # not found 
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
getinfo.shape('data/scotBNG.shp')



# 3. Raster File Formats


# 3.1 Using GDAL Drivers in rgdal
auck.el1 <- readGDAL('data/70042108.tif')
summary(auck.el1)
is.na(auck.el1$band1) <- auck.el1$band1 <= 0 | auck.el1$band1 > 10000

x <- GDAL.open('data/70042108.tif')
(xx <- getDriver(x))
getDriverLongName(xx)
x
GDALinfo('data/70042108.tif')

brks <- c(0, 10, 20, 50, 100, 150, 200, 300, 400, 500, 600, 700)
(pal <- terrain.colors(11))
auck.el1$band1 <- findInterval(auck.el1$band1, vec=brks, all.inside=T) - 1
writeGDAL(auck.el1, 
          'data/demIndex.tif', 
          drivername='GTiff', 
          type='Byte', 
          colorTable=list(pal),
          mvFlag=length(brks) - 1)
Gi <- GDALinfo('data/demIndex.tif', returnColorTable=T)
CT <- attr(Gi, 'ColorTable')[[1]]
CT[CT > '#000000']

#log.zinc <- idw(log(zinc) ~ 1, meuse, meuse.grid)['var1.pred']


# 3.2 Other Import/Export Functions