## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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