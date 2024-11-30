# Computes the inverse of a matrix from a makeCacheMatrix object. Retrieves
# the cached inverse if available; otherwise, calculates and caches it.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y       
    inv <<- NULL  
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# Creates a special "matrix" object that stores a matrix and its cached inverse,
# with methods to set/get the matrix and cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
