# Two functions that implement matrix inversion with caching
# That is, if a matrix has already been inverted, it will be read from cache
# instead of being inverted again

# makeCacheMatrix(x)
# Argument - matrix x to create CacheMatrix object from
# This function constructs the CacheMatrix object, and will do the following:
# 1) Set a matrix
# 2) Get a matrix
# 3) Set the inverse matrix
# 4) Get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <- function(inv) i <<- inv
  getInv <- function() i
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


# cacheSolve(x, ...)
# Argument - matrix x to find an inverse for
# This function calculates the inverse of a given matix
# If the inverse already exists, read it
# If it does not exist, calculate it

cacheSolve <- function(x, ...) {
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
}
