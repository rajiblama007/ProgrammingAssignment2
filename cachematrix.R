## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
        i <- NULL
        set <- function(ma) {
                m <<- ma
                i <<- NULL
        }
        get  <- function() {m}
        setinverse  <- function(inverse) {i <<- inverse}
        getinverse  <- function() {i}
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(m, ...) {
        i <- m$getinverse()
        if(!is.null(i)) {
                message("Getting cached data")
                return(i)
        }
        data <- m$get()
        i <- solve(data, ...)
        m$setinverse(i)
        i
} 