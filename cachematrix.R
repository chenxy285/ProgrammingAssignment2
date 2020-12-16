##Caching the inverse of a matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.



## This function creates a special "matrix" object that can cache its inverse.
## It is a list containing a function to:
## 1) set the value of the vector
## 2) get the value of the vector
## 3) set the value of the mean
## 4) get the value of the mean

## Unlike the usual single arrow assignment (<-) that always works on the 
## current level, the double arrow operator (<<-) can modify variables in 
## parent levels.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse) m<<-inverse
  getinverse<-function() m
  list(sey=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}




## ## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinverse(m)
  m
}




