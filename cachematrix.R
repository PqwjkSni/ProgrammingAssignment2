## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly.
## Following are a pair of functions that cache the inverse of a matrix.
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve will retrieve the inverse from the cache.

## makeCacheMatrix function creates a special "matrix" object
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setsolve<-function(solve) m<<-solve
  getsolve<-function() m
  list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then 
## cacheSolve will retrieve the inverse from the cache.
cacheSolve<-function(x,...){
  m<-x$getsolve()
  if(!is.null(m)){
    message('getting cached data')
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setsolve(m)
  m
}

## To test if the program works, run the following lines.
## When cacheSolve funciton is called the second time, 
## the message "getting cached data" should be displayed 
## before the output is printed.
mymatrix<-makeCacheMatrix(matrix(1:4,2,2))
cacheSolve(mymatrix)
cacheSolve(mymatrix)
