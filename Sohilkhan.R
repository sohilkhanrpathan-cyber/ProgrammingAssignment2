## cachematrix.R
## This script contains functions to cache the inverse of a matrix
## to avoid repeated computation, demonstrating lexical scoping in R.

## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # initialize inverse as NULL
  
  # setter for the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # getter for the matrix
  get <- function() x
  
  # setter for the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # getter for the inverse
  getinverse <- function() inv
  
  # return a list of all four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  # check if inverse is already cached
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if not cached, compute the inverse
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}

## Example usage:
## myMatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## cacheSolve(myMatrix)       # computes inverse
## cacheSolve(myMatrix)       # retrieves cached inverse
