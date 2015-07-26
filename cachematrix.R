## These functions are part of Coursera's R Programming assingment 2
## The purpose is to show cashing of the inverse of a matrix


## This function creates an object Matrix that caches it's Inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function 'solve' the inverse of a Matrix. If the inverse has already been 
## calculated, besides the matrix didn't change, the cachesolve function should 
## retrieve the cached inverse Matrix (in memory).
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverso <- x$getinverse()
    if(!is.null(inverso)) {
        message("getting cached matrix")
        return(inverso)
    }
    data <- x$get()
    inverso <- solve(data, ...)
    x$setinverse(inverso)
    inverso
}
