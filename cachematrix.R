## Put comments here that give an overall description of what your
## functions do

## This function creates a special matrix that can cache its inverse 

## makeCacheMatrix() takes a matrix as its parameter and returns
## a List of functions for manipulating the matrix and its
## cached inversed, which is set or retrieved by cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        ## Clearing cache and create set/get functions
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinv <- function(xinv) {
                inverse <<- xinv
        }
        getinv <- function() inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}



## This function calculates the inverse of the matric from the function makeCacheMatrix.  If the inverse is already calculated for the same
## matrix, then cacheSolve will retrieve theh inverse from the cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
        inverse <- x$getinv()
        if (!is.null(inverse)) {
                message("getting cached inverse")
                return(inverse)
        }
        message("calculating inverse")
        mat <- x$get()
        inverse <- solve(mat, ...)
        x$setinv(inv)
        inverse
}
