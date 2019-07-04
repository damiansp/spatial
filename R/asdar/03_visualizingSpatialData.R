#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/spatial/R/asdar/')

library(classInt)
library(ggplot2)
library(grid)
library(lattice)
library(latticeExtra)
library(maps)
library(maptools)
library(RColorBrewer)
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
box()
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
# 2.1 Straight trellice example
levelplot(z ~ x + y | name, spmap.to.lev([c('direct', 'log')]), asp='iso')
spplot(zn[c('direct', 'log')])

# 2.2 Plotting points, lines, polygons and grids
data(meuse.grid)
coordinates(meuse.grid) <- c('x', 'y')
meuse.grid <- as(meuse.grid, 'SpatialPixelsDataFrame')
img <- as.image.SpatialGridDataFrame(meuse.grid['dist'])
contour.lines <- ContourLines2SLDF(contourLines(img))
spplot(contour.lines)

# 2.3 Adding reference and layout elements to plots
river <- list('sp.polygons', meuse.poly)
north <- list('SpatialPolygonsRescale', 
              layout.north.arrow(), 
              offset=c(178750, 332500), 
              scale=400)
scale <- list('SpatialPolygonsRescale', 
              layout.scale.bar(), 
              offset=c(180200, 329800), 
              scale=1000, 
              fill=c('transparent', 'black'))
txt1 <- list('sp.text', c(180200, 329950), '0')
txt2 <- list('sp.text', c(181200, 329950), '1 km')
pts <- list('sp.points', meuse, pch=3, col=1)
meuse.layout <- list(river, north, scale, txt1, txt2, pts)
spplot(zn['log'], sp.layout=meuse.layout)



# 3. Alternative Routes: ggplot, latticeExtra
methods(fortify)
m <- as(meuse, 'data.frame')
ggplot(m, aes(x, y)) + geom_point() + coord_equal()

p <- spplot(meuse['zinc'])
m <- SpatialPolygonsDataFrame(meuse.poly, data.frame(col=1), match.ID=F)
l <- spplot(m)
l + p
p + l



# 4. Interactive Plots
# 4.1 Interacting with base graphics
plot(meuse)
meuse.id <- identify(coordinates(meuse))

region <- locator(type='o')
n <- length(region$x)
p <- Polygon(cbind(region$x, region$y)[c(1:n, 1), ], hole=F)
ps <- Polygons(list(p), ID='region')
sps <- SpatialPolygons(list(ps))
plot(meuse)
plot(meuse[sps, ], pch=16, col=2, add=T)

prj <- CRS('+proj=longlat +datum=NAD27')
nc.shp <- system.file('shapes/sids.shp', package='maptools')[1]
nc <- readShapePoly(nc.shp, proj4string=prj)
plot(nc)
pt <- locator(type='p')
print(pt)
pt.sp <- SpatialPoints(cbind(pt$x, pt$y), proj4string=prj)
over(pt.sp, nc)

# 4.2 Interacting with spplot and lattice plots
ids <- spplot(meuse, 'zinc', identify=T)
trellis.focus('panel', column=1, row=1)
ids <- panel.identify()
trellis.unfocus()

trellis.focus('panel', column=1, row=1)
as.numeric(grid.locator())
trellis.unfocus()



# 5. Color Palettes and Class Intervals
# 5.1 Color palettes
ry.colors <- colorRampPalette(c('red', 'yellow'))
image(meuse.grid, meuse.grid['dist'], col=ry.colors(10))
example(brewer.pal)

# 5.2 Class intervals
pal <- brewer.pal(5, 'Reds')
q5 <- classIntervals(meuse$zinc, n=5, style='quantile')
q5
diff(q5$brks)
plot(q5, pal=pal)

fj5 <- classIntervals(meuse$zinc, n=5, style='fisher')
fj5
diff(fj5$brks)
plot(fj5, pal=pal)

q5.colors <- findColours(q5, pal)
par(mar=rep(0, 4))
plot(meuse, col=q5.colors, pch=19)
legend('topleft', 
       fill=attr(q5.colors, 'palette'), 
       legend=names(attr(q5.colors, 'table')), 
       bty='n')