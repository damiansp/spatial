#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/spatial/R/asdar')

library(maptools)
library(rgdal)
library(spatstat)
data(japanesepines)



# 2. Packages for the Analysis of Spatial Point Patterns
summary(japanesepines)
spjpines <- as(japanesepines, 'SpatialPoints')
summary(spjpines)
spjpines1 <- elide(spjpines, scale=T, unitsq=T) # convert coord dims to unit sq.
summary(spjpines1)

pppjap <- as(spjpines1, 'ppp')
summary(pppjap)

spasthma <- readOGR('./data/spasthma.shp', 'spasthma')
spbdry <- readOGR('./data/spbdry.shp', 'spbdry')
spsrc <- readOGR('./data/spsrc.shp', 'spsrc')
sproads <- readOGR('./data/sproads.shp', 'sproads')

