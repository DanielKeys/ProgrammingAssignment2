## These functions find the inverse of a matrix and cache it, or get the cache if
## it already exists and the matrix hasn't changed

## makes a list of functions for dealing with the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## checks list to see if cache exists, else finds inverse and caches it

cacheSolve <- function(m, ...) {
        i <- m$getinverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- m$get()
        i <- solve(data, ...)
        m$setinverse(i)
        i
}
