## Like many other computation process, 
## inverse of matrix is highly time-consuming computations process, specially
## for higher order matrixes and with large number of itterations.
## In this  programming assignment an R function has been written 
## that is able to cache potentially time-consuming inverse of matrix computations. 
## If the contents of a Matrix are not changing, then it will
## cache the value of the mean for further utilization when we need it again,
## it can be looked up in the cache rather than recomputed. 
## In this Programming Assignment we take the advantages 
## of the scoping rules of the R language.
## 

## The first function, makeCacheMatrix creates a special "vector",
## which is really a list containing a function to
##
## setmatrix set the value of the matrix
## getmatrix: get the value of the vector
## setinv: set the value of the inverse
## getinv: get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inm <- NULL
  
  ## setmatrix function: set the global value for given matrix 
  
  setmatrix <- function(y) {
    x <<- y
    inm <<- NULL
  }
  
  getmatrix <- function() x
  setinverse <- function(inverse) inm <<- inverse
  getinverse <- function() inm
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The 2nd function calculates the inverse of the special "vector" 
## created with the above first function. However, it first checks to see if the 
## inverse of matrix has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates the mean 
## of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inm <- x$getinverse()
  if(!is.null(inm)) {
    message("getting cached inverse of matrix")
    return(inm)
  }
  data <- x$getmatrix()
  inm <- solve(data,...)
  x$setinverse(inm)
  inm
}