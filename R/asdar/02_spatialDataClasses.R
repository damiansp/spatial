rm(list=ls())
setwd('~/Learning/spatial/R/asdar')

library(maps)
library(maptools)
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