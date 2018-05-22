#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/spatial/R/spatialAnalysisForMapping')

library(GISTools)

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