## Objective: Caching the Inverse of a Matrix as part of the Coursera R Programming week 3 assignment
## Author: Ganesh
## March 2016

## This function will create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  Inv_Mat <- NULL
  
  set <- function(y) {
    x <<- y
    Inv_Mat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) Inv_Mat <<- inverse
  getinverse <- function() Inv_Mat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
