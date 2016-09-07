## This series of functions creates a "matrix" object (really a list), 
## calculates its inverse,and then caches the result, so that the inverse 
## does not have to be computed repeatedly.


# makeCacheMatrix "creates" a matrix that can cache its inverse by
# creating a list containing functions that:
# 1) Set an invertible matrix
# 2) Get an invertible matrix
# 3) Set the matrix inverse
# 4) Get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  IV <- NULL
  setM <- function(y){
    x <<- y
    IV <<- NULL
  }
  getM <- function() x
  setIV <- function(inverse) IV <<- inverse
  getIV <- function() IV
  list(setM=setM, getM=getM, setIV=setIV, getIV=getIV)
}


# cacheSolve calculates the inverse of the matrices created above:
# Contains functions that
# 1) Check to see if the inverse of a matrix has already been calculated. Skips computation and 
# retrieves from cache if so
# 2) Calculates inverse of matrices returned by makeCacheMatrix

cacheSolve <- function(x,...) {
  IV <- x$getIV()
  if(!is.null(IV)){
    message("Getting cached data")
    return(IV)
  }
  IVdata <- x$getM()
  IV <- solve(IVdata, ...)
  x$setIV(IV)
  IV
}