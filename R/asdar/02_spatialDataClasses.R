rm(list=ls())
setwd('~/Learning/spatial/R/asdar')

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
CRAN.df <- read.table('data/CRAN051001a.txt', header=T)
CRAN.mat <- cbind(CRAN.df$long, CRAN.df$lat)
row.names(CRAN.mat) <- 1:nrow(CRAN.mat)
str(CRAN.mat)
getClass('SpatialPoints')
llCRS <- CRS('+proj=longlat +ellps=WGS84')
CRAN.sp <- SpatialPoints(CRAN.mat, proj4string=llCRS)
summary(CRAN.sp)

# 4.1 Methods
bbox(CRAN.sp)
proj4string(CRAN.sp)
brazil <- which(CRAN.df$loc == 'Brazil')
brazil
coordinates(CRAN.sp)[brazil,]
summary(CRAN.sp[brazil,])
southern.hemi <- which(coordinates(CRAN.sp)[, 2] < 0)
summary(CRAN.sp[-southern.hemi, ])
