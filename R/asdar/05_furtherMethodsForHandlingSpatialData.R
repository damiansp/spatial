#=========#=========#=========#=========#=========#=========#=========#=========
rm(list=ls())
lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=''),
       detach,
       character.only=T,
       unload=T)
setwd('~/Learning/spatial/R/asdar')

library(rgeos)
library(testthat)

# 2. Handling and Combining Features


# 2.1 The rgeos package
getScale()
set_do_poly_check(F)
test_package('rgeos')
set_do_poly_check(T)


# 2.2 Using rgeos

