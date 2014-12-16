## A pair of functions that cache the inverse of a matrix to avoid potentially 
## time-consuming computations. Specifically I am using solve() to find the 
## inverse of a matrix and cache it using a free floating variable. 
## I am also trying to cache the input matrix so that I can retrieve it and 
## compare it to any new input matrices.

## The first function, makeCacheMatrix creates a special "matrix" object that can 
## cache the input matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # sets the value of inv to NULL 
  setmatrix <- function(y) {  # set the value of the matrix
    x <<- y  # caches the inputted matrix so that cacheSolve can check whether it 
             # has changed (note this is within the setmatrix function)
    inv <<- NULL # sets the value of inv (the matrix inverse if used cacheSolve) to NULL
  } 
  getmatrix <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function()inv
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse) # creates a list to house the four functions
}


## This function computes the inverse of the special "matrix" returned 
## by the function makeCacheMatrix (above). 
## If the inverse has already been calculated (and the matrix has not 
## changed), then cacheSolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve 
## function in R. For example, if X is a square invertible matrix, then 
## solve(X) returns its inverse.

cacheSolve <- function(x, ...) { # Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() # if an inverse has already been calculated this gets it
  if(!is.null(inv)) { # check to see if cacheSolve has been run before
    message("getting cached data")
    return(inv)
  }
  data <- x$getmatrix()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


