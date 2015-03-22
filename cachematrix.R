## These 2 functions will cache the inverse of a matrix. First a special
## "matrix" is created, which is able to cache its inverse and the second
## function calculates the inverse if it was not yet calculated.
## These functions were tested as:
## > mat<-makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))
## > cacheSolve(mat)
## First calling of makeCacheMatrix will set inverse to Null.
## The inverse is calculated after first calling cacheSolve, all next calls
## with the same variable will give the message "getting cached data"
## If we introduce new variable or we change the matrix e.g.:
## > mat<-makeCacheMatrix(matrix(6:9,nrow=2,ncol=2))
## makeCacheMatrix will have a different environment and calling cacheSolve(mat)
## will newly calculate the inverse for this matrix and environment.

## makeCacheMatrix creates a special "matrix" - actually a list of functions
## which set and get the value of the matrix and set and get it's inverse.
## The output of this function is a list of function with the actual environment

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function is calculating the inverse of the matrix, but first
## it checks if the inverse for that actual matrix was not yet calculated
## The x in this function represents the output of makeCacheMatrix function
## (which we called with desired variable-matrix for which we want to 
## calculate the inverse) - so x in cacheSolve function is a list of functions
## within certain environment.


cacheSolve <- function(x, ...) {
  
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
