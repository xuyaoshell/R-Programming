# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the 
  # matrix. Contains the following functions:
  # * set      set the value of a matrix
  # * get      get the value of a matrix
  # * setinv   set the cached value (inverse of the matrix)
  # * getinv   get the cached value (inverse of the matrix)
  #
  # Notes:
  # not sure how the "x = numeric()" part works in the argument list of the 
  # function, but it seems to be creating a variable "x" that is not reachable 
  # from the global environment, but is available in the environment of the 
  # makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
        x <<- y
        inv <<- NULL
      }
      get <- function() x
      setinv <- function(solve) inv <<- solve
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      as.matrix(inv)
}
