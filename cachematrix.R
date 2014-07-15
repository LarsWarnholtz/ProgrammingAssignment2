## The following two functions calculate the inverse of a matrix
## and cache the result so that it can be reused when the same
## matrix inverse is required again instead of being calculated again.

## makeCacheMatrix takes a matrix and produces a list of four functions
## that can be used to get and set the matrix itself as well as its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve takes a matrix that has been constructed using makeCacheMatrix and
## checks if the inverse of this matrix has already been cached. If so, it returns
## the cached values otherwise it calculates the inverse and caches it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
