## Valentin BONNEAUD
## Pair of functions that compute and cache the inverse of a matrix for accelerate
## computations in loops (for example)

## This function creates a special "matrix" object that can cache its inverse. 
##
## There is no need to call the set method during the creation of the object
## because the attribute x is set by the declaration of the makeCacheMatrix function
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        ## we reset the inverse to recompute it because the matrix has changed
        inverse <<- NULL 
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function to get the inverse of the given matrix
##
## Compute the inverse with the solve function only the first time and then the
## next time return the cached inverse
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    
    ## if the inverse is already computed, we return the cached inverse
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## otherwise we compute the inverse with the solve function
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
