## makeCacheMatrix creates a special "matrix", 

## The function contains the function to
## set -- set the matrix value, 
## get -- get the matrix, 
## getInv -- get the inverse of the matrix
## setInv -- set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) x <<- y
    get <- function() x
    getInv <- function() inv
    setInv <- function(inputInv) inv <<- inputInv
    list(set = set, get = get, getInv = getInv, setInv=setInv)
}


## This function returns the iverse of a function.
## In case the inverse was already calculated the function get the value from the cache

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
