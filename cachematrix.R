## makeCacheMatrx & cacheSolve work together return the inverse of
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
    setSolve <- function(inverse) m <<- inverse
    getSolve <- function() m
    list(set = set, get = get, 
               setSolve = setSolve, getSolve = getSolve)
}

## cacheSolve returns the invertible matrix derived from
## the special matrix returned from makeCacheMatrix
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
