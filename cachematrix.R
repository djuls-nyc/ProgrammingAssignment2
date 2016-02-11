## The functions below allow to cache the inverse of a matrix
##
## makeCacheMatrix creates a special matrix object
## that can cache its inverse
##
## cacheSolve gives access to the inverse matrix


## makeCacheMatrix creates a special "matrix" object
## The matrix itself is initialized with the value
## of the parameter x
## The set function sets the matrix to a new value.
## The get function returns the matrix (base class)

makeCacheMatrix <- function(x = matrix()) {
  
  ## Set the cache i to NULL to flag nothing is cached
  i <- NULL
  
  ## the set function gives a new value to x, and reset i to NULL
  ## so it flags a new invert needs to be computed and cached
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## just return the x matrix
  get <- function() x
  
  ## function called by cacheSolve to store / cache 
  ## the computed inversed matrix
  setinv <- function(inv) i <<- inv
  
  ## just return the cache inversed matrix
  getinv <- function() i
  
  ## Upon completion of makeCacheMatrix, return the
  ## list of functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## cacheSolve(M,...) returns the inverse matrix of
## the special "matrix" object M, but only computes it
## if it hasn't been cached before.

cacheSolve <- function(x, ...) {
  
  ## first checks if the inverse is not already stored
  i <- x$getinv()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## compute the inverse matrix
  data <- x$get()
  i <- solve(data, diag(1, dim(data)[1], dim(data)[2]),...)
  
  ## cache the inverse
  x$setinv(i)
  
  i  
}
