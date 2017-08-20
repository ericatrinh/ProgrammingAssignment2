## makeCacheMatrx & cacheSolve work together to return the inverse of
## a square matrix from the cache

## makeCacheMatrix contains functions to set/get the matrix and to set/get 
## the values from solve() to cache the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get, 
               setSolve = setSolve, getSolve = getSolve)
}

## cacheSolve takes in the object returned from makeCacheMatrix and returns 
## the invertible matrix 
cacheSolve <- function(x, ...) {
    m <- x$getSolve()
    if( !is.null(m) ){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve( data, ... )
    x$setSolve(m)
    m
}
