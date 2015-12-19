##Matrix inversion is usually a costly computation and there may be some benefit to 
##caching the inverse of a matrix rather than computing it repeatedly (there are also alternatives to matrix inversion
##that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

## 1.makeCacheMatrix : This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseMatrix) inv <<- inverseMatrix
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## 2. cacheSolve : This function computes the inverse of the special "matrix" returned by  makeCacheMatrix  above. If the inverse has already been calculated 
##    (and the matrix has not changed), then  cacheSolve  should retrieve the inverse from the cache.

## For this assignment, we assume that the matrix supplied is always invertible.
 
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  print(inv)
  if(!is.null(inv)) {
    print("ok")
    message("getting cached data")
    return(inv)
  }
  message("caching data")
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

## Example :
##source("makeCacheMatrix.R")
##source("cacheSolve.R")
