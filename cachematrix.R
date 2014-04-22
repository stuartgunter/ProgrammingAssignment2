## These functions provide the ability to 'wrap' a matrix for the purposes of caching.
## When used together, they allow a matrix to be defined and its inverse cached while
## the matrix remains unchanged.


## makeCacheMatrix wraps a matrix in a list that provides 'accessors' for getting and setting
## the matrix as well as its inverse. The inverse is not pre-calculated, but may be cached
## after it has been requested (via cacheSolve). If the matrix changes at any point, the cached
## inverse will be reset and will need to be re-calculated.
makeCacheMatrix <- function(x = matrix()) {
  cached <- NULL
  set <- function(y) {
    x <<- y
    cached <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cached <<- inverse
  getInverse <- function() cached
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve accepts a wrapped matrix (created with makeCacheMatrix) and returns its inverse.
## On first execution, the matrix inverse will be calculated and cached within the wrapped matrix.
## Subsequent requests for the inverse of the matrix will use the previously calculated value from
## the cache.
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("Using cached inverse")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
