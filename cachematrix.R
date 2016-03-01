## Objective: Caching the Inverse of a Matrix as part of the Coursera R Programming week 3 assignment
## Author: Ganesh
## March 2016

## This function will create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  Inv_Mat <- NULL #Redefine the inverse matrix to make sure we re-calculate the inverse
  
  set <- function(y) {
    x <<- y
    Inv_Mat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) Inv_Mat <<- inverse #Assigning the inverse matrix to be stored in the environment.
  getinverse <- function() Inv_Mat #Retrieve the inverse matrix
  #Create list with all the functions created
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  #Get the inverse value and store it in a variable
  inverse <- x$getinverse()
  #Verify whether the variable is having a valid value.
  #If it is not null, then it means that the value already exists (cached) which can be reused
  if(!is.null(inverse)) {
  #Print the message that we are retrieving the value from a cache instead of calculating it again
    message("getting cached data")
  #Exit the function by returning the cached value instead of calculating again.
    return(inverse)
  }
  #Control arrives here if no cached value is found
  data <- x$get() #Invoke the get function in makeCacheMatrix to get the input matrix
  inverse <- solve(data, ...) #Process the inverse matrix for the input matrix
  x$setinverse(inverse) #Store the calculated inverse matrix in the environment to be cached and re-used for next execution.
  inverse #Return the inverse matrix to the calling function
}
