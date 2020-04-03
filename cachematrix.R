##  Matrix inversion, using caching
##  Author:   MO
##  Version:  1    
##  Date:     3.4.2020

# copy from example

## Create a "Matrix" object with a cacheable inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <-function(y) {
    x <<- y
    inverse <<- NULL
    

  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list (set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Return the inverse of a matrix, utilising the cache if available
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if (!is.null(inverse)) {
    message("Getting cached data")
    return(inverse) 
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setinverse(inverse)
  inverse
}

