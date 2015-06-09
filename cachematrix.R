## Functions to cakculate the inverse of a matrix and cache it.

## Creates a wrapper object for a matrix and its inverse, with get and set methods
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Performs the inversion operation, where input is the wrapper obtained from
## makeCacheMatrix() above; if inverse has been calculated and cached, simply
## returns it; otherwise, recalculate.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached inverse")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat)
    x$setinverse(inv)
    inv
}
