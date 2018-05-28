#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/spatial/R/spatialAnalysisForMapping')

library(GISTools)
library(rgeos)
data(georgia)
data(newhaven)
data(tornados)

# 2. Spatial Intersection or Clip Operations
par(mar=rep(0, 4))
plot(us_states)
plot(torn, pch=16, add=T, col='#FB6A4A33', cex=0.4) # tornado data
plot(us_states, add=T)
summary(torn)

midwest <- (us_states$STATE_NAME == 'Illinois'
            | us_states$STATE_NAME == 'Wisconsin'
            | us_states$STATE_NAME == 'Iowa'
            | us_states$STATE_NAME == 'Missouri'
            | us_states$STATE_NAME ==  'Indiana')
midwest <- us_states[midwest, ]
plot(midwest)
plot(torn, pch=16, add=T, col='#FB6A4A33')

# mask tornado with midwest
midwest.torn <- gIntersection(midwest, torn)
plot(midwest)
plot(midwest.torn, add=T, pch=16, col='#FB6A4A33')

# preserve data attributes
midwest.torn <- gIntersection(midwest, torn, byid=T)
head(midwest.torn)
tmp <- rownames(data.frame(midwest.torn))
tmp <- strsplit(tmp, ' ')
torn.id <- (sapply(tmp, '[[', 2))
state.id <- (sapply(tmp, '[[', 1))
torn.id <- as.numeric(torn.id)
df1 <- data.frame(torn[torn.id, ])

df2 <- us_states$STATE_NAME[as.numeric(state.id)]
df <- cbind(df2, df1)
head(df)
names(df)[1] <- 'State'
midwest.torn <- SpatialPointsDataFrame(midwest.torn, data=df)
index2 <- match(df2, us_states$STATE_NAME)
df3 <- data.frame(us_states)[index2, ]
df3 <- cbind(df2, df1, df3)
names(df3)[1] <- 'State'
head(df3)
midwest.torn <- SpatialPointsDataFrame(midwest.torn, data=df3)



# 3. Buffers
texas <- us_states2[us_states2$STATE_NAME == 'Texas', ]
plot(texas)
tex.buffer <- gBuffer(texas, width=25000) # width in m (25km)
par(mar=rep(0, 4))
plot(tex.buffer, col=4, border=4)
plot(texas, add=T, col='white')

georgia.county.buffer <- gBuffer(georgia2, width=5000, byid=T, id=georgia2$Name)
plot(georgia.county.buffer, col=4)
plot(georgia2, add=T, border='white')



# 4. Merging Spatial Features
usa <- gUnaryUnion(us_states)
plot(us_states, border='darkgreen', lty=4 )
plot(usa, add=T, border='darkgreen', lwd=3)



# 5. Point-In-Polgon and Area Calculations
# 5.1 Point-in-polygon
tornado.count <- poly.counts(torn, us_states)
par(mar=c(4, 4, 3, 1))
hist(tornado.count)
rug(tornado.count)

# 5.2 Area calculations
proj4string(us_states2) # note: units in m
poly.areas(us_states2) # area per state in sq. m
poly.areas(us_states2) / (100^2)  # hectares
poly.areas(us_states2) / (1000^2) # sq. km

# 5.3 Point and areas anlysis exercise
densities <- poly.counts(breach, blocks) / ft2miles(ft2miles(poly.areas(blocks)))
cor(blocks$P_OWNEROCC, densities)
plot(blocks$P_OWNEROCC, densities)
n.breaches <- poly.counts(breach, blocks)
area <- ft2miles(ft2miles(poly.areas(blocks)))
pois.mod <- glm(
  n.breaches ~ P_OWNEROCC, offset=log(area), data=blocks, family=poisson)
summary(pois.mod)
par(mfrow=c(2, 2))
plot(pois.mod)
s.resids <- rstandard(pois.mod)
resid.shades <- shading(c(-2, 2), c('red', 'grey', 'blue'))
par(mfrow=c(1, 1), mar=rep(0, 4))
choropleth(blocks, s.resids, resid.shades)
pois.mod <- update(pois.mod, .~. + P_VACANT)
par(mfrow=c(2, 2))
plot(pois.mod)
s.resids <- rstandard(pois.mod)
par(mfrow=c(1, 1), mar=rep(0, 4))
choropleth(blocks, s.resids, resid.shades)
par(mar=c(5, 4, 4, 2))



# 6. Creating Disturbance Attributes
proj4string(places) <- CRS(proj4string(blocks))
centroids <- gCentroid(blocks, byid=T, id=rownames(blocks))
centroids <- spTransform(centroids, CRS(proj4string(blocks)))
plot(blocks)
points(centroids, pch=16, cex=0.6)
distances <- ft2miles(gDistance(places, centroids, byid=T))
distances <- gWithinDistance(places, blocks, byid=T, dist=miles2ft(1.2))

