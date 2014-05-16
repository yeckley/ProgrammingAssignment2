## Programming Assignment 2 for JHU Intro to R cours via Coursera. This 
## assignmnet demonstrates the use of lexical scoping to implement a caching
## scheme to improve efficiency of R programs that may need to repeat
## computationally expensive tasks.

## It assumes the some environment will call makeCacheMatrix() from an
## environment (e.g. the global environment) assign the returned list to some
## object, which is then passed to cacheSolve().

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## when setting we no longer have a cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(x) {
    message("That's new. I'll save that.",appendLF=TRUE)
    inv <<- x
  }
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Get the inverse of a matrix. If the matrix is already cached
## use that to save some cycles. Otherwise, calculate it and 
## cache it.

cacheSolve <- function(x) {
  ## Check whether the cached inverse is non-null and, if so, return it.
  if (!is.null(x$getinv())) {
      message('Returning cached matrix.', appendLF=TRUE)
      return(x$getinv())
    }
  ## If the inverse matrix cache was empty, set it now and then return it.
  x$setinv(solve(x$get()))
  x$getinv()
}        