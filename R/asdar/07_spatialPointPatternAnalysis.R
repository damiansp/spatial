#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/spatial/R/asdar')

library(cubature)
library(lattice)
library(maptools)
library(rgdal)
library(spatstat)
library(splancs)
data(cells)
data(japanesepines)
data(lansing)
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


# 4.3 Estimation of the Intensity
mserwq <- mse2d(as.points(coordinates(spred)), 
                as.points(list(x=c(0, 1, 1, 0), y=c(0, 0, 1, 1))), 
                100, 
                0.15)
bwq <- mserwq$h[which.min(mserwq$mse)] # select bandwidth that minimizes mse
bwq # 0.039

# alternately
mserw <- as.numeric(bw.diggle(as(spred, 'ppp')))
mserw # 0.0198
bw <- as.numeric(mserw)

plot(mserwq$mse ~ mserwq$h, 
     type='l', 
     xlab='bandwidth', 
     ylab='MSE', 
     main='Quartic Kernel')
points(mserwq$h[which.min(mserwq$mse)], min(mserwq$mse))

poly <- as.points(list(x=c(0, 0, 1, 1), y=c(0, 1, 1, 0)))
sG <- Sobj_SpatialGrid(spred, maxDim=100)$SG
grd <- slot(sG, 'grid')
summary(grd)
k0 <- spkernel2d(spred, poly, h0=bw, grd)
k1 <- spkernel2d(spred, poly, h0=0.05, grd)
k2 <- spkernel2d(spred, poly, h0=0.1, grd)
k3 <- spkernel2d(spred, poly, h0=0.15, grd)
df <- data.frame(k0=k0, k1=k1, k2=k2, k3=k3)
head(df)
kernels <- SpatialGridDataFrame(grd, data=df)
summary(kernels)

cc <- coordinates(kernels)
xy <- list(x=cc[, 1], y=cc[, 2])
k4 <- density(as(spred, 'ppp'), 0.5 * bw, dimyx=c(100, 100), xy=xy)
plot(k4)
kernels$k4 <- as(k4, 'SpatialGridDataFrame')$v
k5 <- density(as(spred, 'ppp'), 0.5 * 0.05, dimyx=c(100, 100), xy=xy)
plot(k5)
kernels$k5 <- as(k5, 'SpatialGridDataFrame')$v
k6 <- density(as(spred, 'ppp'), 0.5 * 0.1, dimyx=c(100, 100), xy=xy)
plot(k6)
kernels$k6 <- as(k6, 'SpatialGridDataFrame')$v
k7 <- density(as(spred, 'ppp'), 0.5 * 0.15, dimyx=c(100, 100), xy=xy)
plot(k7)
kernels$k7 <- as(k7, 'SpatialGridDataFrame')$v
summary(kernels)


# 4.4 Likelihood of an Imhomogeneous Poisson Process
log.lambda <- function(x, alpha, beta) {
  alpha + sum(beta * c(x, x * x, prod(x)))
}

L <- function(alphabeta, x) {
  l <- apply(x, 1, log.lambda, alpha=alphabeta[1], beta=alphabeta[-1])
  l <- sum(l)
  int.L <- adaptIntegrate(
    lowerLimit=c(0, 0), 
    upperLimit=c(1, 1), 
    fDim=1, 
    tol=1e-08, 
    f=function(x, alpha=alphabeta[1], beta=alphabeta[-1]) { 
      exp(log.lambda(x, alpha, beta))
    })
  l - int.L$integral
}

l.maple <- lansing[lansing$marks == 'maple', ]
x <- as.points(l.maple)
opt.beta <- optim(
  par=c(log(514), 0, 0, 0, 0, 0), fn=L, control=list(maxit=1000, fnscale=-1), x=x)
opt.beta

ppm.mod <- ppm(Q=l.maple, trend=~x + y + I(x^2) + I(y^2) + I(x * y))
ppm.mod
par(mfrow=c(3, 4))
par(mar=c(0.1, 0.1, 4, 1.5))
plot(ppm.mod)


# 4.5 Second-Order Properties
Kenv.jap <- envelope(as(spjpines1, 'ppp'), fun=Kest, r=r, nrank=2, nsim=99)
Kenv.red <- envelope(as(spred, 'ppp'), fun=Kest, r=r, nrank=2, nsim=99)
Kenv.cells <- envelope(as(spcells, 'ppp'), fun=Kest, r=r, nrank=2, nsim=99)
K.results <- rbind(Kenv.jap, Kenv.red, Kenv.cells)
K.results <- cbind(K.results, 
                   y=rep(c('Japanese', 'Redwood', 'Cells'), each=length(r)))



# 5 Some Applications of Spatial Epidemiology


# 5.1 Case-Control Studies

# 5.1.1 Spatial Variation of the Relative Risk
bw.asthma <- 0.06
ppp.asthma <- as(spasthma, 'ppp')
ppp.asthma$window <- as(spbdry, 'owin')
marks(ppp.asthma) <- relevel(ppp.asthma$marks$Asthma, 'control')

cases <- unmark(subset(ppp.asthma, marks(ppp.asthma) == 'case'))
n.cases <- npoints(cases)
controls <- unmark(subset(ppp.asthma, marks(ppp.asthma) == 'control'))
n.controls <- npoints(controls)
k.cases <- density(cases, bw.asthma)
k.controls <- density(controls, bw.asthma)
par(mfrow=c(2, 1))
plot(k.cases)
plot(k.controls)

sp.k.ratio0 <- as(k.cases, 'SpatialGridDataFrame')
names(sp.k.ratio0) <- 'k.cases'
sp.k.ratio0$k.controls <- as(k.controls, 'SpatialGridDataFrame')$v
sp.k.ratio <- as(sp.k.ratio0, 'SpatialPixelsDataFrame')
sp.k.ratio$k.ratio <- sp.k.ratio$k.cases / sp.k.ratio$k.controls
sp.k.ratio$log.ratio <- log(sp.k.ratio$k.ratio) - log(n.cases / n.controls)
plot(sp.k.ratio)

iters <- 99
ratio <- rep(NA, iters)
p.val.map <- rep(0, nrow(sp.k.ratio))
r.label.ratio <- matrix(NA, nrow=iters, ncol=nrow(sp.k.ratio))

for (i in 1:iters) {
  if (i %% 5 == 0 ) cat(sprintf('iter: %d\r', i))
  ppp.asthma0 <- rlabel(ppp.asthma)
  cases.rel <- unmark(subset(ppp.asthma0, marks(ppp.asthma0) == 'case'))
  controls.rel <- unmark(subset(ppp.asthma0, marks(ppp.asthma0) == 'control'))
  k.cases.rel <- density(cases.rel, bw.asthma)
  k.controls.rel <- density(controls.rel, bw.asthma)
  k.ratio.rel <- eval.im(k.cases.rel / k.controls.rel)
  r.label.ratio[i, ] <- as(as(k.ratio.rel, 'SpatialGridDataFrame'), 
                           'SpatialPixelsDataFrame')$v
  p.val.map <- p.val.map + (sp.k.ratio$k.ratio < r.label.ratio[i, ])
}

cell.size <- k.controls$xstep * k.controls$ystep
ratio.rho <- cell.size * sum((sp.k.ratio$k.ratio - n.cases / n.controls)^2)
ratio <- cell.size * apply(
  r.label.ratio, 
  1, 
  function(x, rho0) {
  	sum((x - rho0)^2)
  },
  rho0=n.cases / n.controls)
(p.value.rho <- (sum(ratio > ratio.rho) + 1) / (iters + 1)) 
# 0.56 - not-signif: consistent with constant-risk ratio (e.g., risk of asthma const 
# at all locations)

sp.k.ratio$p.val.map <- (p.val.map + 1) / (iters + 1)
img.p.val <- as.image.SpatialGridDataFrame(sp.k.ratio['p.val.map'])
clp.val <- contourLines(img.p.val, levels=c(0, 0.05, 0.95, 1))
c1 <- ContourLines2SLDF(clp.val)
image(img.p.val)
lines(c1)


# 5.2 Binary Regression Estimator
rr.bw <- bw.relrisk(ppp.asthma, hmax=0.5)
rr.bw # bandwidh of 0.209 by this criterion
rr <- relrisk(ppp.asthma, rr.bw)
rr2 <- relrisk(ppp.asthma, bw.asthma)
sp.k.ratio$prob <- as(as(rr, 'SpatialGridDataFrame'), 'SpatialPixelsDataFrame')$v
sp.k.ratio$prob2 <- as(as(rr, 'SpatialGridDataFrame'), 'SpatialPixelsDataFrame')$v

