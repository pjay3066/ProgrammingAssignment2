##turns a mtrix into a list of functions that set or get the
##data in the matrix and set or get it's inverse

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

##Solves the inverse of a matrix. If the inverse has already
##been calculated, it skips the solve function an retrieves
##the inverse from the cache

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
}
