#---------#---------#---------#---------#---------#---------#---------#---------
rm(list=ls())
setwd('~/Learning/spatial/R/rsam/')


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