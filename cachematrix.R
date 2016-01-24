## cachematrix.R
##
## This files contains two functions to cache the result of a matrix inverse calculation.
##
## Usage
##   create a wrapper object for a matrix with the makeCacheMatrix() function
##   calculate the inverse with the cacheSolve() function
##   calling the function cacheSolve() again will retrieve the result from the cache
##
## Example usage
##  m <- makeCacheMatrix(matrix(rnorm(9), 3, 3))
##  s <- cacheSolve(m) # calculate the inverse
##  s <- cacheSolve(m) # get result from cache
##

## Description
##   Wrapper function for a matrix that allows for caching of the matrix inverse
##
## Usage
##   makeCacheMatrix(x)
##
## Arguments
##  x: a square numeric or complex matrix containing the
##     coefficients of the linear system.
##
## Setter functions
##   set: sets matrix
##   setInverse: sets matrix inverse
##
## Getter functions
##   get: returns matrix
##   getInverse: returns matrix inverse
##
makeCacheMatrix <- function(x = matrix()) {
  inverse = NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(y) inverse <<- y
  getInverse <- function() inverse
  list(set = set, 
       get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}

## Description
##   This function calculates the inverse of a matrix.
##
##   This function assumes that the matrix supplied is always invertible.
##
##   If the function was already called for the same makeCacheMatrix object
##   the result will be retained from it's cache
##
## Usage
##   cacheSolve(x, ...) {
##
## Arguments
##  x: an object with a getInverse() function
##
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("Result from cached data.")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
