## This uses R's "poor man's object orientation" to create
## something resembling a class that is initialized with a 
## matrix. It has a getter and setter for the initial matrix,
## as well as a "inverse" field, which can be used to store
## the inverse matrix by the cacheSolve function (using the
## getinverse getter and setinverse setter)
## All in all it resembles the makeVector function from the
## assignment.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}


## This is a wrapper for the makeCacheMatrix function
## If the inverse of the given makeCacheMatrix was 
## calculated before,it will just return the stored version 
## using "getinverse"
## Otherwise it will calculate the inverse and store it,
## before returning it.
## It resembles the "cachemean" function from the assignment.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("Returning cached data")
    return(inverse)
  }
  matr <- x$get()
  inverse <- solve(matr)
  x$setinverse(inverse)
  inverse
}

## Test it like this:
## > source("cacheMatrix.R")
## > mat <- matrix(c(1,2,3,4), nrow=2, ncol=2)
## > cm <- makeCacheMatrix()
## > cm$set(mat)
## > cacheSolve(cm)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(cm)
## Returning cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


