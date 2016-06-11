## cachematrix.R
##
## This file contains functions to find the inverse of a matrix
## and cache the value to optimise future calls 

## This function creates a list to cache the result of inverting
## the matrix passed to it
makeCacheMatrix <- function(x = matrix()) {
  # m will hold the cached inverted matrix 
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setSolve <- function(inverted) im <<- inverted
  getSolve <- function() im
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}

## This function returns the inverse of the matrix passed in
## x using a cached result if called with x previously
##
## x - a matrix constructed using makeCacheMatrix 
cacheSolve <- function(x, ...) {
  # try and get a cached inverted matrix
  im <- x$getSolve()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  # couldn't find a cached value so need to calculate the
  # inverse then cache it
  data <- x$get()
  im <- solve(data, ...)
  x$setSolve(im)
  ## Return a matrix that is the inverse of 'x'
  im
}
