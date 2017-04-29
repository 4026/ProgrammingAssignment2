## Functions to create a special matrix class that caches the value of its 
## inverse, and a function that calculates the inverse of such a matrix or 
## returns a cached value if it was already calculated.

## Create a special type of matrix that is able to cache the value of its 
## inverse.
makeCacheMatrix <- function(m = matrix()) {
    inverse <- NULL
    set <- function(new_m) {
        m <<- new_m
        inverse <<- NULL
    }
    get <- function() m
    setinverse <- function(new_inverse) inverse <<- new_inverse
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns a matrix that is the inverse of 'm', which is an instance of the 
## special matrix class created by the makeCacheMatrix function. If the provided
## matrix has had its inverse claculated by this function before, a cached 
## result will be returned instead of recalulating the inverse.
cacheSolve <- function(m, ...) {
    cached_value <- m$getinverse()
    if(!is.null(cached_value)) {
        message("getting cached data")
        return(cached_value)
    }
    data <- m$get()
    
    ## We disregard any additional arguments passed to cacheSolve, rather than 
    ## passing them through to solve, because passing through additional 
    ## arguments might cause us to get an output from solve that is not the 
    ## inverse of the provided matrix, which would in turn lead us to cache that
    ## value and return it for later calls to cacheSolve, which would then also 
    ## be rendered incorrect.
    inverse <- solve(data) 
    m$setinverse(inverse)
    inverse
}
