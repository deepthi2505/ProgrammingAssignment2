## The following two functions are used to create a special object that 
## stores a matrix and caches its inverse of the matrix.
## If there exists cached inverse, it returns the same. Otherwise, calculates the 
## inverse of the matrix and returns it.


## The function makeCacheMatrix() creates a special matrix object which creates a 
## list of functions to set and get the values of the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The function cacheSolve() returns the inverse of the matirx. 
## If the inverse of the matrix is already calculated, this function retrieves the
## same from cache and if not, calculates the inverse of the matrix and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("Getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
