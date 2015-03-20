## The first function caches the inverse of a matrix.
## The second function looks for the cached inverse, returns it if found, computes it otherwise.

## This functions create a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This looks for the cached inverse. If it finds one, it returns the inverse.
## Otherwise, it computes the inverse.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {                 
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
## Return a matrix that is the inverse of 'x'