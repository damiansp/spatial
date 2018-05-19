rm(list=ls())
setwd('~/Learning/spatial/R/spatialAnalysisForMapping')

library(GISTools)

data(georgia)


# 5 Writing Functions for Spatial Data
plot(georgia.polys[[1]], asp=1, type='l')

# 5.1 Drawing polygons in a list
plot(c(939200, 1419420), c(905510, 1405900), asp=1, type='n')
lapply(georgia.polys, polygon)
invisible(lapply(georgia.polys, polygon)) # prevents ouput


# 5.2 Automatically choose bbox
poly1 <- georgia.polys[[1]]
min(poly1[, 1])

get.eastmost <- function(polys) { 
  eastmost.list <- lapply(polys, function(poly) { min(poly[, 1]) })
  min(unlist(eastmost.list))
}

get.eastmost(georgia.polys)

