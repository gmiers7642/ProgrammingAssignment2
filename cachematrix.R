# Two functions that implement matrix inversion with caching
# That is, if a matrix has already been inverted, it will be read from cache
# instead of being inverted again

# This function constructs the CacheMatrix object, and will do the following:
# 1) Set a matrix
# 2) Get a matrix
# 3) Set the inverse matrix
# 4) Get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    y <<- NULL
  }
  get() <- function() x
  setInverse() <- function(inv) i <<- inv
  getInverse() <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# This function calculates the inverse of a given matix
# If the inverse already exists, read it
# If it does not exist, calculate it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x&getInverse()
  if(!is.null(i) ) {
    message("Getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setIverse(i)
  i
}
