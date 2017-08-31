##
## makeCacheMatrix creates a special "vector", which is really a list containing a function to the following:
##  set the value of the vector
##  get the value of the vector
##  set the value of the inverse
##  get the value of the inverse
##
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inv <<- i
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##
## cacheSolve calculates the inverse of the matrix found within the the special "vector" created with the above function.
## It first checks the cache to determine if it has already been computed.
## If found, it returns the result from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and stores it in the cache before returning the result.
##
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("found cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
