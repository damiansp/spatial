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

get.bounds <- function(polys) { 
  eastmost.list <- lapply(polys, function(poly) { min(poly[, 1]) })
  westmost.list <- lapply(polys, function(poly) { max(poly[, 1]) })
  ew <- c(min(unlist(eastmost.list)), max(unlist(westmost.list)))
  northmost.list <- lapply(polys, function(poly) { max(poly[1, ]) })
  southmost.list <- lapply(polys, function(poly) { min(poly[1, ]) })
  ns <- c(min(unlist(southmost.list)), max(unlist(northmost.list)))
  list(ew=ew, ns=ns)
}

ga.bounds <- get.bounds(georgia.polys)

# 5.3 Shaded Maps
classifier <- factor(ifelse(georgia$PctRural > 50, 'rural', 'urban'))
fill.cols <- character(length(classifier))
fill.cols[classifier == 'urban'] <- 'yellow'
fill.cols[classifier == 'rural'] <- 'darkgreen'
par(mar=rep(0, 4))
plot(ga.bounds$ew, ga.bounds$ns, asp=1, type='n')
invisible(mapply(polygon, georgia.polys, col=fill.cols))