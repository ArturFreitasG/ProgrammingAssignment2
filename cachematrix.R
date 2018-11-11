## The functions below have the general purpose of obtaining the inverse matrix of Matrix "x"

## Function "makeCacheMatrix" is be used to cache the inverse of Matrix "x", to be used in future occasions.
## The goal is to save computing resources in those future situations.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function() inv <<- solve(x)
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function "cacheSolve" is used to return the previously cached inverse of matrix "x".
## In cases when the inverse of matrix "x" has been previously cached (that is, if loop is True), the computing effort is significantly lower,
## than if the inverse had to be calculated, as happens in "makeCacheMatrix" function above.
## If the inverse of matrix "x" is not previously cached (that is, if loop is False), then it is calculated in the section after the if loop.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
