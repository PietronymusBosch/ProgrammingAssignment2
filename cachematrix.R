## Allows matrix inverses to be cached locally after solving
## to remove the cost of future computations.

## Makes a matrix whose inverse can be cached with cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = setinverse)
}


## Finds the inverse of the matrix given.
## If not yet calculated, solves and caches result,
## else returns saved result.

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(x)
  x$setmean(inverse)
  inverse
}
