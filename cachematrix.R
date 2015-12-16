## These functions provide the functionality to efficiently perform
## matrix inversion by caching the results of previous calculations

## This function creates a list that acts as a wrapper around a matrix.
## It is able to store and retrieve both the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function()
        x
    setinverse <- function(i)
        inverse <<- i
    
    
    getinverse <- function()
        inverse
    list(
        set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
    
}


## This function calculates the inverse of a matrix that is contained in 
## the cacheMatrix. If the inverse has already been calcualted it will return
## the cached result, otherwise it will calculate the inverse and store it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    message("calculating inverse")
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
