## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## Here is a pair of functions that cache the inverse of a matrix.

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
## The function makeCacheMatrix creates a special "vector", which is really a 
## list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of inverse of a square matrix 
## - get the value of inverse of a square matrix 

makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL
        set<-function(y){
                x<<-y
                inverse<<-NULL
        }
        get<-function() x
        setinv<-function(inv) inverse<<-inv
        getinv<-function() inverse
        list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache

cacheSolve <- function(x, ...) {
        inverse<-x$getinv()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        data<-x$get()
        inverse<-solve(data,...)
        x$setinv(inverse)
        inverse
        ## Return a matrix that is the inverse of 'x'
}

## My results
## > source("cachematrix.R")
## > M<-matrix(1:4,nrow=2,ncol=2)
## > M
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > st_M<-makeCacheMatrix(M)
## > cacheSolve(st_M)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(st_M)
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > M%*%cacheSolve(st_M)
## getting cached data
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## > 
