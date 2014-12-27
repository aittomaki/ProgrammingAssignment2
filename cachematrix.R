## Functions for matrices that have a cached inverse matrix.
## Allows using cached values for matrix inversions so they don't have
## to be recalculated every time an inverse is needed.
## This is a solution to Programming Assignment 2 of the Coursera
## course R Programming.



## Creates a special matrix, that is a list of four functions:
##     set sets the value of stored the matrix
##     get returns the value of the matrix
##     setinverse sets the inverse for the matrix (shouldn't be used directly)
##     getinverse returns the inverse of the stored matrix

makeCacheMatrix <- function(x = matrix()) {
    # Create a variable for storing the inverse
    i <- NULL

    # Function for setting the value of the matrix
    set <- function(y) {
        x <<- y
        # Reset the inverse
        i <<- NULL
    }
    # Function for getting the matrix itself
    get <- function() x
    
    # Function for setting the inverse of the matrix
    setinverse <- function(y) i <<- y
    
    # Function for getting the inverse
    getinverse <- function() i
    
    # Return a list with the functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## Solves the inverse for a given cacheMatrix (created with
## the createCacheMatrix function) and caches it.
## If the inverse has already been solved and cached,
## returns the cached value.

cacheSolve <- function(x, ...) {
    
    # Get the inverse from x and return if it is defined
    i <- x$getinverse()
    if(!is.null(i)) {
        message("Returning cached inverse.")
        return(i)
    }
    # Solve and cache the inverse if it hasn't been defined
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
