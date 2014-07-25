##The functions will cache the inverse of a matrix to not compute it 
## if not necessary

## makeCacheMatrix
## This function creates a special "matrix" object that can cache
## the inverse
## It's a list containing a function to set the value of the matrix
## get the value, set the Inv and get the Inv.

makeCacheMatrix <- function(x = matrix()) {
  m <-NULL
  
  set<-function(y){
      x <<- y
      m <<- NULL  
  }
  get <- function() x
  setInvMat<- function(solve) m<<- solve
  getInvMat<- function() m
  list(set=set,get=get,
       setInvMat=setInvMat,
       getInvMat=getInvMat)
}

## The function computes the inverse of the special matrix returned by
## makeCacheMatrix and if the inverse has already been calculated
## and the matrix has not changed then the chachesolve retrieve
## the inverse from the cache
cacheSolve <- function(x, ...) {

  m <-x$getInvMat()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInvMat(m)
  m
  
}
