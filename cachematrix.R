## makeCacheMatrix produces a special "matrix" object which caches its inverse 
## for the input (i.e. an square matrix which we assume can be inverted).


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The function cacheSolve computes the inverse of the aforementioned matrix 
## returned by makeCacheMatrix. However, when the inverse has already been 
## calculated and the matrix remained unchanged, cacheSolve will instead
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

