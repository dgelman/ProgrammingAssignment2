## This first function creates makeCacheMatrix, creates a special "vector", which is really a list
## containing 4 functions:
## to set the value of the matrix, get the value of the matrix, set the value of the inverse matrix
## and get the value of the inverse matrix
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

## The cacheSolve function calculates the inverse of the matrix contained in the
## special "vector" created with the above function. However, it first checks to
## see if the inverse matrix has already been calculated. If so, it `get`s the inv
## matrix from the cache and skips the computation. Otherwise, it calculates the
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


