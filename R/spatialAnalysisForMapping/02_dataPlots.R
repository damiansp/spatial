#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/spatial/R/rsam/')

library(GISTools)
library(RColorBrewer)

data(georgia)
data(meuse.grid)

# 2.3 Data types and classes

# 2.3.2 Data classes
# Defining your own classes
employee <- list(name='Lex Luthor', start.year=2005, position='Mastermind')
class(employee) <- 'staff'

print.staff <- function(x) {
  cat('Name: ', x$name, 
      '\nStart Year: ', x$start.year, 
      '\nJob Title: ', x$position, '\n')
}

print(employee)

# unclass(employee) # back to list
# rm(print.staff)

# Classes in lists
new.staff <- function(name, year, post) {
  result <- list(name=name, start.year=year, position=post)
  class(result) <- 'staff'
  result
}

famb <- vector(mode='list', 3)
famb[[1]] <- new.staff('McKenzie', 1975, 'jie')
famb[[2]] <- new.staff('Abner', 1980, 'di')
famb[[3]] <- new.staff('Ellen', 1982, 'mei')
famb




# 4 Plots
# 4.1 Basic plot tools
appling <- georgia.polys[[1]]
plot(appling, asp=1, type='n', xlab='Easting', ylab='Northing')
#polygon(appling, density=14, angle=145)
polygon(appling, col=rgb(0, 0.5, 0.7, 0.4))

mat <- SpatialPixelsDataFrame(points=meuse.grid[c('x', 'y')], data=meuse.grid)

par(mfrow=c(1, 2))
par(mar=rep(0, 4))
image(mat, 'dist')
greens <- brewer.pal(14, 'Greens')
image(mat, 'dist', col=greens)
par(mfrow=c(1, 1))



# 5 Reading, Writing, Loading, Saving
# 5.2 R data files
# save(list=c('obj1', 'obj2, '...'), file='file/path.RData')
# load('filepath.RData')

# 5.3 Spatial data files
#writePolyShape(georgia, 'path/georgia.shp')
#georgia <- readShapePoly('path/georgia.shp')


