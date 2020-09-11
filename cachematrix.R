## This program will cache the calculation of the inverse
## of a matrix, which may be a time-consuming calculation.
## Takes advantage of scoping rules in R.

## This function input is a square, invertible matrix.
## It returns a list of functions that will:
## set the matrix, get the matrix, set the inverse, get the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
    ## <<- will search parent environment to assign variable
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## This function takes the output of cacheMatrix and returns the 
## inverse of the original matrix. If the inverse has been solved,
## the result is returned from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  return(inv)
}


