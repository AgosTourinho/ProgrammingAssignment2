## Inversion of large matrices can be computationally expensive.
## The following functions are intended to generate special objects
## which allow the inverse of a matrix to be cached, so that once
## computed for the first time, it can be retrieved from memory thus
## saving computational effort.

## makeCacheMatrix creates an object which contains a matrix and
## can cache the value of its inverse.
## Returns a list containing the following functions:
##    set: stores a matrix in the object and clears the
##      cached inverse.
##    get: retrieves the stored matrix.
##    setInverse: stores the inverse of the matrix in the cache.
##    getInverse: retrieves the inverse of the matrix if previously
##      cached, or NULL otherwise.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve takes an object created with makeCacheMatrix and
## returns the inverse of the matrix stored in the object.
## If the inverse has previously been cached, the function retrieves
## the cached value. Otherwise, it computes the inverse and stores
## its value into the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
  
}
