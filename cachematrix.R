## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("Returning cached inverse.")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
