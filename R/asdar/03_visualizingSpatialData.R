rm(list=ls())
setwd('~/Learning/spatial/R/asdar/')

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