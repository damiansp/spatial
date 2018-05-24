rm(list=ls())
setwd('~/Learning/spatial/R/asdar/')

library(lattice)
library(maps)
library(maptools)
library(rgdal)
library(sp)
data(meuse)
data(meuse.grid)
data(meuse.riv)

# 1 Traditional Plot Systme
# 1.1 Plotting points, lines, polygons and grids
coordinates(meuse) <- c('x', 'y')
plot(meuse)

cc <- coordinates(meuse)
class(cc)
cc <- cc[order(cc[, 'x'], cc[, 'y']), ]
meuse.sl <- SpatialLines(list(Lines(list(Line(cc)), 'line1')))
plot(meuse.sl)

meuse.list <- list(Polygons(list(Polygon(meuse.riv)), 'meuse.riv'))
meuse.poly <- SpatialPolygons(meuse.list)
plot(meuse.poly, col=4, add=T)

coordinates(meuse.grid) <- c('x', 'y')
meuse.grid <- as(meuse.grid, 'SpatialPixels')
image(meuse.grid, col='darkgrey')
plot(meuse.poly, col=rgb(0, 0, 1, 0.8), add=T)
plot(meuse, pch=16, col=rgb(0, 0.5, 0, 0.8), cex=0.75, add=T)

# 1.2 Axes and layout elements
layout(matrix(c(1, 2), 1))
plot(meuse.poly, axes=T)
plot(meuse.poly, axes=F)
axis(1, at=c(178000 + 0:2*2000), cex.axis=0.7)
axis(2, at=c(326000 + 0:3*4000), cex.axis=0.7)
# box()

oldpar <- par(no.readonly=T)
plot(meuse, axes=T, cex=0.6)
plot(meuse.poly, add=T, col=4)

par(mar=rep(0.1, 4))
plot(meuse, axes=F, cex=0.7)
plot(meuse.poly, add=T, col=4)
box()
par(oldpar)

par(mfrow=c(1, 1))
plot(meuse)
plot(meuse.poly, col=4, add=T)
SpatialPolygonsRescale(layout.scale.bar(), 
                       offset=locator(1), 
                       scale=1000, 
                       fill=c('transparent', 'black'), 
                       plot.grid=F) # click map to add scale bar
text(locator(1), '0')
text(locator(1), '1 km')
SpatialPolygonsRescale(layout.north.arrow(), 
                       offset=locator(1), 
                       scale=400, 
                       plot.grid=F) # click map to add arrow
box()

# 1.3 Degrees in Axes Labels and Reference Grid
world <- map('world', interior=F, xlim=c(-179, 179), ylim=c(-89, 89), plot=T)
world.p <- pruneMap(world, xlim=c(-179, 179))
ll.CRS <- CRS('+proj=longlat +ellps=WGS84')
world.sp <- map2SpatialLines(world.p, proj4string=ll.CRS)
proj.new <- CRS('+proj=moll')
world.proj <- spTransform(world.sp, proj.new)
world.grid <- gridlines(world.sp, 
                        easts=c(-179, seq(-150, 150, 50), 179.5),
                        norths=seq(-75, 75, 15),
                        ndiscr=100)
world.grid.proj <- spTransform(world.grid, proj.new)
at.sp <- gridat(world.sp, easts=0, norths=seq(-75, 75, 15), offset=0.3)
at.proj <- spTransform(at.sp, proj.new)
plot(world.proj, col='grey30')
plot(world.grid.proj, col='grey70', add=T)
text(coordinates(at.proj), 
     pos=at.proj$pos, 
     offset=at.proj$offset, 
     labels=parse(text=as.character(at.proj$labels)), 
     cex=0.6)
     
# 1.4 Plot Size, Plotting Area, Map Scale, and Multiple Plots
# Set figure size in inches
par('pin')
# par(pin=c(4, 4))
# dev.off()
# quartz(width=10, height=10)
# pdf('file.pdf', width=5, height=7)
# par(mfrow=c(2, 3)) # SAME AS
#layout(matrix(1:6, 2, 3, byrow=T))

# 1.5 Plotting Attributes and Map Legends
greys <- grey.colors(4, 0.55, 0.95)
#image(zn.idw, col=greys, breaks=log(c(100, 200, 400, 800, 1800)))
#plot(meuse.pol, add=T)
#plot(meuse, cex=sqrt(measure$zinc) / 20, add=T)

# 2 Trellis/Lattice Plots with spplot
