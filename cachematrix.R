## cachematrix.R
## Utility functions for creating and caching a matrix and it's inverse.
## makeCacheMatrix stores the matrix and it's inverse
## cacheSolve calculates the inverse, if necessary, and returns it's inverse.
## If the inverse has already been calculated, then cacheSolve returns
## the previously calculated inverse.


## makeCacheMatrix stores the matrix x and it's inverse and provides
## utility functions to get and set the matrix and it's inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinv <- function(y) {
    inverse <<- y
  }
  
  getinv <- function() {
    inverse
  }
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}

## cacheSolve accepts a cacheMatrix x and returns it's inverse.
## If the inverse has previously been calculated, then it's cached
## version is returned.  Otherwise, the inverse is calculated, cached, 
## and returned.
cacheSolve <- function(x, ...) {

  inverse <- x$getinv()
  
  if(is.null(inverse)) {
    data <- x$get()

    inverse <- solve(data, ...)
    x$setinv(inverse)
  }
  
  inverse

}
