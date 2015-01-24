
## makeCacheMatrix() takes a matrix as its parameter and returns a List of functions for manipulating the matrix and its
## cached inversed, which is set or retrieved by cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        ## Clearing cache and create set/get functions
        inverse <- NULL             
        set <- function(y) {    ## set function. clears inverse. sets x to input matrix 
                        x <<- y
                        inverse <<- NULL
                }         
        get <- function() {     ## get function retrieves x             
                        x
                }
        setinv <- function(xinv) {   ## setinv function sets inverse cache based in calculated inverse of x
                inverse <<- xinv
                }
        getinv <- function() {  ## getinv function retrieves inverse matrix    
                inverse
                }
        list(set = set, get = get, setinv = setinv, getinv = getinv)     ## makeCachematrix returning list of the functions 
       
}

## This function calculates the inverse of the matric from the function makeCacheMatrix.  If the inverse is already calculated for the same
## matrix, then cacheSolve will retrieve theh inverse from the cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
        inverse <- x$getinv()      ##applies the getinv function to get inverse of x. assignes to inverse 
        
        if (!is.null(inverse)) {    ## if inverse hasnt been calculated it will be null. If it is NOT null, we return inverse. 
                        message("getting cached inverse")
                        return(inverse)
                }
        message("calculating inverse")   ## if inverse IS null, we need to calculate inverse 
        matrix <- x$get()                   ## retrieve matrix   
        inverse <- solve(matrix, ...)       ## calculate inverse and set to inverse variable 
        x$setinv(inverse)                ## records inverse as cached
        inverse                          ## returns the inverse matrix, retrieved from cache or calculated  
}
