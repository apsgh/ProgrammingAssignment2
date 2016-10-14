##  The following functions can compute and cache the inverse of a matrix.


## makeCacheMatrix(): creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
  inverseMAt <- NULL                     
  ##set the value of the matrix
  set <- function(y) {                      
    x <<- y
    inverseMat <<- NULL              
  }
  ## get the value of the matrix
  get <- function() x                           
  #Set the inverse of  matrix with solve function
  setinverse <- function(solve) inverseMAt <<- solve 
  # Get the value of the matrix
  getinverse <- function() inverseMAt        
  list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)
}
##
## cacheSolve(): computes the inverse of the "matrix" returned by makeCacheMatrix(). If
##               the inverse has already been calculated and the matrix has not changed,
##               it'll retrieves the inverse from the cache directly.

cacheSolve<- function(x, ...) {                 
  inverseMat <- x$getinverse()
  #if the inverse exists
  if(!is.null(inverseMat)) {                 
    message("getting cached data")
    return(inverseMat)
  }
  #if the inverse does not exist then calculate inverse
  data <- x$get()                               
  inverseMat <- solve(data, ...)
  x$setinverse(inverseMat)
  inverseMat
}