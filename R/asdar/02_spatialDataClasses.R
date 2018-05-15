rm(list=ls())
setwd('~/Learning/spatial/R/asdar')

library(maps)
library(maptools)
library(rgeos)
library(sp)


# 3. Spatial Objects
getClass('Spatial')
getClass('CRS')

m <- matrix(c(0, 0, 1, 1), ncol=2, dimnames=list(NULL, c('min', 'max')))
crs <- CRS(projargs=as.character(NA))
S <- Spatial(bbox=m, proj4string=crs)

bb <- matrix(c(350, 85, 370, 95), ncol=2, dimnames=list(NULL, c('min', 'max')))
Spatial(bb, proj4string=CRS('+proj=longlat'))



# 4. Spatial Points
cran.df <- read.table('data/CRAN051001a.txt', header=T)
cran.mat <- cbind(cran.df$long, cran.df$lat)
row.names(cran.mat) <- 1:nrow(cran.mat)
str(cran.mat)
getClass('SpatialPoints')
llCRS <- CRS('+proj=longlat +ellps=WGS84')
cran.sp <- SpatialPoints(cran.mat, proj4string=llCRS)
summary(cran.sp)

# 4.1 Methods
bbox(cran.sp)
proj4string(cran.sp)
brazil <- which(cran.df$loc == 'Brazil')
brazil
coordinates(cran.sp)[brazil,]
summary(cran.sp[brazil,])
southern.hemi <- which(coordinates(cran.sp)[, 2] < 0)
summary(cran.sp[-southern.hemi, ])

# 4.2 Dataframes for Spatial Point Data
str(row.names(cran.df))
cran.spdf1 <- SpatialPointsDataFrame(
  cran.mat, cran.df, proj4string=llCRS, match.ID=T)
str(cran.spdf1)
cran.spdf1[4,]
cran.spdf1@data[4, ]
cran.spdf1$loc # or [['loc']]
s <- sample(nrow(cran.df))
cran.spdf2 <- SpatialPointsDataFrame(
  cran.mat, cran.df[s,], proj4string=llCRS, match.ID=T)
all.equal(cran.spdf1, cran.spdf2)

cran.df1 <- cran.df
row.names(cran.df1) <- sample(c(outer(letters, letters, paste, sep='')), 
                              nrow(cran.df1))
cran.spdf3 <- SpatialPointsDataFrame(
  cran.mat, cran.df1, proj4string=llCRS, match.ID=T) # Throws error
  
getClass('SpatialPointsDataFrame')
names(cran.spdf1)
str(model.frame(lat ~ long, data=cran.spdf1), give.attr=F)

cran.spdf4 <- SpatialPointsDataFrame(cran.sp, cran.df)
all.equal(cran.spdf4, cran.spdf2)

cran.df0 <- cran.df
coordinates(cran.df0) <- cran.mat
proj4string(cran.df0) <- llCRS
all.equal(cran.df0, cran.spdf2)

str(cran.df0, max.level=2)
cran.df1 <- cran.df
names(cran.df1)
coordinates(cran.df1) <- c('long', 'lat')
proj4string <- llCRS
str(cran.df1, max.level=2)

turtle.df <- read.csv('data/seamap105_mod.csv')
head(turtle.df)
timestamp <- as.POSIXlt(
  strptime(as.character(turtle.df$obs_date), '%m/%d/%Y %H:%M:%S'), 
  'GMT')
turtle.df1 <- data.frame(turtle.df, timestamp=timestamp)
turtle.df1$lon <- ifelse(turtle.df1$lon < 0, turtle.df1$lon + 360, turtle.df$lon)
turtle.sp <- turtle.df1[order(turtle.df1$timestamp), ]
coordinates(turtle.sp) <- c('lon', 'lat')
proj4string(turtle.sp) <- CRS('+proj=longlat +ellps=WGS84')
plot(turtle.sp)
map('world', add=T)


# 5 SpatialLines
getClass('Line')
getClass('Lines')
getClass('SpatialLines')

japan <- map('world', 'japan', plot=F)
p4s <- CRS('+proj=longlat +ellps=WGS84')
SLjapan <- map2SpatialLines(japan, proj4string=p4s)
str(SLjapan, max.level=2)
plot(SLjapan)

lines.len <- sapply(slot(SLjapan, 'lines'), function(x) length(slot(x, 'Lines')))
table(lines.len)

volcano.s1 <- ContourLines2SLDF(contourLines(volcano))
plot(volcano.s1)
t(slot(volcano.s1, 'data'))

llCRS <- CRS('+proj=longlat +ellps=WGS84')
auck.shore <- MapGen2SL('data/auckland_mapgen.dat', llCRS)
plot(auck.shore)
summary(auck.shore)


# 6 SpatialPolygons
lns <- slot(auck.shore, 'lines')
islands.auck <- sapply(
  lns,
  function(x) {
    crds <- slot(slot(x, 'Lines')[[1]], 'coords')
    identical(crds[1, ], crds[nrow(crds), ])
  })
table(islands.auck)

getClass('Polygon')
getClass('Polygons')
getClass('SpatialPolygons')

islands.sl <- auck.shore[islands.auck]
plot(auck.shore)
plot(islands.sl, add=T, col=4)
list.of.lines <- slot(islands.sl, 'lines')
islands.sp <- SpatialPolygons(
  lapply(
    list.of.lines,
    function(x) {
  	  Polygons(list(Polygon(slot(slot(x, 'Lines')[[1]], 'coords'))), 
  	           ID=slot(x, 'ID'))
    }),
  proj4string=CRS('+proj=longlat +ellps=WGS84'))
summary(islands.sp)
plot(auck.shore)
plot(islands.sp, col=4, border=5, add=T)
slot(islands.sp, 'plotOrder')
order(
  sapply(
    slot(islands.sp, 'polygons'), 
    function (x) {
  	  slot(x, 'area')
    }),
  decreasing=T)
  
# 6.1 SpatialPolygonsDataFrame Objects
state.map <- map('state', plot=F, fill=T)
ids <- sapply(strsplit(state.map$names, ':'), function(x) { x[1] })
state.sp <- map2SpatialPolygons(state.map, IDs=ids, proj4string=llCRS)
plot(state.sp)

sat <- read.table('data/state.sat.data_mod.txt', row.names=5, header=T)
head(sat)
id <- match(row.names(sat), row.names(state.sp))
row.names(sat)[is.na(id)]
sat1 <- sat[!is.na(id), ]
state.spdf <- SpatialPolygonsDataFrame(state.sp, sat1)
head(slot(state.spdf, 'data'))
str(state.spdf, max.level=2)
#rownames(sat1)[2] <- 'Arizona'
#SpatialPolygonsDataFrame(state.sp, sat1) # no longer matches
DC <- 'district of columbia'
not.dc <- !(row.names(state.spdf) == DC)
state.spdf1 <- state.spdf[not.dc, ]
dim(state.spdf1) # 48 continental states

# 6.2 Holes and Ring Direction
# Cannot locate data source: manitoulin_sp
length(slot(islands.sp, 'polygons'))
sapply(
  slot(slot(islands.sp, 'polygons')[[1]], 'Polygons'),
  function (x) {
  	slot(x, 'ringDir')
  })
islands.sp <- createSPComment(islands.sp)
sapply(slot(islands.sp, 'polygons'), comment)



# 7 SpatialGrid and SpatialPixel Objects
getClass('GridTopology')
bb <- bbox(islands.sp)
bb
cs <- c(0.01, 0.01)
cc <- bb[, 1] + (cs / 2)
cd <- ceiling(diff(t(bb)) / cs)
islands.grid <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
islands.grid

getClass('SpatialGrid')
p4s <- CRS(proj4string(islands.sp))
islands.sg <- SpatialGrid(islands.grid, proj4string=p4s)
summary(islands.sg)
plot(islands.sg) # just a grid
#class(auck_el1) # More missing data
#object.size(auck_el1)
#object.size(slot(auck_el1, 'data))
