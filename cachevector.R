## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## The following function calculates the inverse of the matrix contained in the
## special "vector" created with the above function. However, it first checks to
## see if the inverse matrix has already been calculated. If so, it `get`s the inv
## matrix from the ## cache and skips the computation. Otherwise, it calculates the
## inverse of the matrix, sets the value in the cache via the 'setinv' function.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}


