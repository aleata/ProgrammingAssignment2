# This script contains two functions that (a) store an object representing
# a matrix and its mutator functions and (b) compute the inverse of a matrix 
# or retrieve the matrix inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
  # Creates an object that stores a matrix and its inverse
  #
  # Args:
  #   x: A square matrix
  #
  # Returns:
  #   A list of 4 functions that:
  #   (1) set the value of a matrix, 
  #   (2) get the value of the matrix,
  #   (3) set the value of the matrix inverse,
  #   (4) get the value of the matrix inverse
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
If the inverse has already been calculated (and the matrix has not changed), 
then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Compute or retrieve from the cache the inverse of the matrix returned by a makeCacheMatrix object
  #
  # Args:
  #   x: A makeCacheMatrix object
  #
  # Returns:
  #   The inverse of the matrix returned by x
  #   If the inverse is retrieved from the cache, a message alerting the user is printed
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}