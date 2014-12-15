## makeCachMatrix and cacheSolve are a pair of functions that can calculate
## and cache the inverse of a matrix.
## If the matrix has been already calculated, then the result is returned from the
## cache.

## makeCacheMatrix creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix(), ...) {
        matrix(x, ...)
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() {matrix(x, ...)}
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() {m}
        list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## cacheSolve computes the inverse of the matrix returned by "makeCacheMatrix."

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        ## Return a matrix that is the inverse of 'x'
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
