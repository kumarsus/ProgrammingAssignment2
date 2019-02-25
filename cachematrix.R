## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  + i=NULL
  + set<-function(y){
    + x<<-y
    + i<<-NULL
  }
  + get<-function()x
  + setinverse<-function(inverse)i<<-inverse
  + getinverse<-function()i
  + list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  + i<-x$getinverse
  + if(!ls.null(i)){
    + message("getting cached data")
    + return(i)
  }
  + Z<-x$get
  + i<-solve(Z,...)
  + x$setinverse(i)
  + i
}
