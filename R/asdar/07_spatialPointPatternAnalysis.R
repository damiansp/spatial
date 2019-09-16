#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/spatial/R/asdar')

library(lattice)
library(maptools)
library(rgdal)
library(spatstat)
data(cells)
data(japanesepines)
data(redwoodfull)




# 2. Packages for the Analysis of Spatial Point Patterns
summary(japanesepines)
spjpines <- as(japanesepines, 'SpatialPoints')
summary(spjpines)
spjpines1 <- elide(spjpines, scale=T, unitsq=T) # convert coord dims to unit sq.
summary(spjpines1)

spred <- as(redwoodfull, 'SpatialPoints')
spcells <- as(cells, 'SpatialPoints')
dpp <- data.frame(
  rbind(coordinates(spjpines1), coordinates(spred), coordinates(spcells)))
njap <- nrow(coordinates(spjpines1))
nred <- nrow(coordinates(spred))
ncells <- nrow(coordinates(spcells))
dpp <- cbind(
  dpp, c(rep('JAPANESE', njap), rep('REDWOOD', nred), rep('CELLS', ncells))) 
names(dpp) <- c('x', 'y', 'DATASET')
xyplot(y ~ x | DATASET, data=dpp, pch=19, aspect=1)

pppjap <- as(spjpines1, 'ppp')
summary(pppjap)

spasthma <- readOGR('./data/spasthma.shp', 'spasthma')
spbdry <- readOGR('./data/spbdry.shp', 'spbdry')
spsrc <- readOGR('./data/spsrc.shp', 'spsrc')
sproads <- readOGR('./data/sproads.shp', 'sproads')

plot(spbdry, axes=T)
plot(sproads, add=T, col='grey30')
plot(spasthma, 
     add=T, 
     pch=c(4, 17)[(spasthma$Asthma == 'case') + 1], 
     col=c(2, 4)[(spasthma$Asthma == 'case') + 1], 
     cex=c(0.6, 0.75)[(spasthma$Asthma == 'case') + 1])
plot(spsrc, pch=22, add=T, cex=1.2, bg=5)



# 3. Preliminary Analysis of Point Pattern


# 3.1. Complete Spatial Randomness


# 3.2. G Function: Distance to the Nearest Event
set.seed(12345)
r <- seq(0, sqrt(2) / 6, by=0.005)
env.jap   <- envelope(as(spjpines1, 'ppp'), fun=Gest, r=r, nrank=2, nsim=99)
env.red   <- envelope(as(spred, 'ppp'),     fun=Gest, r=r, nrank=2, nsim=99)
env.cells <- envelope(as(spcells, 'ppp'),   fun=Gest, r=r, nrank=2, nsim=99)
G.results <- rbind(env.jap, env.red, env.cells)
G.results <- cbind(G.results, 
                   y=rep(c('Japanese', 'Redwood', 'Cells'), each=length(r)))

xyplot(obs ~ theo | y, 
       data=G.results, 
       type='l', 
	   panel=function(x, y, subscripts) {
		 lpolygon(c(x, rev(x)), 
		          c(G.results$lo[subscripts], rev(G.results$hi[subscripts])),
		          border='gray', col='gray')
		 llines(x, y, col='black', lwd=2)
	   })
# Cells: evenly distributed; Japanese: Random; Redwood: clustered


# 3.3 F Function: Distance from a Point to the Nearest Event
r <- seq(0, sqrt(2) / 6, by=0.001)
Fenv.jap   <- envelope(as(spjpines1, 'ppp'), fun=Fest, r=r, nrank=2, nsim=99)
Fenv.cells <- envelope(as(spcells, 'ppp'),   fun=Fest, r=r, nrank=2, nsim=99)
Fenv.red   <- envelope(as(spred, 'ppp'), fun=Fest, r=r, nrank=2, nsim=99)
F.results <- rbind(Fenv.jap, Fenv.red, Fenv.cells)
F.results <- cbind(F.results, 
                   y=rep(c('Japanese', 'Redwood', 'Cells'), each=length(r)))
xyplot(obs ~ theo | y, 
       data=F.results, 
       type='l', 
	   panel=function(x, y, subscripts) {
		 lpolygon(c(x, rev(x)), 
		          c(F.results$lo[subscripts], rev(F.results$hi[subscripts])),
		          border='gray', col='gray')
		 llines(x, y, col='black', lwd=2)
	   })



# 4. Statistical Analysis of Spatial Point Process


# 4.1 Homogeneous Poisson Processes
