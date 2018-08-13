## The functions below perform matrix inverse. In case the matrix inverse has been calculated before
## and the input matrix has not changed, the inverse matrix will be read from cache.

makeCacheMatrix <- function(x = matrix()) {
    ## This function creates a special "matrix" object that can cache its inverse.
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## This function computes the inverse of the special "matrix" returned by 
    ## makeCacheMatrix above. If the inverse has already been calculated 
    ## (and the matrix has not changed), then cacheSolve should retrieve
    ## the inverse from the cache.
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
