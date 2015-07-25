## cache inverse matrix potentially time-consuming computations

##  creates a list of functions to
##  set the value of the matrix
##  get the value of the matrix
##  set the value of the inverse matrix
##  get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    v <- NULL
    set <- function(y) {
        x <<- y
        v <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) v <<- inverse
    getinverse <- function() v
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## uses cache for inverse matrix computations
cacheSolve <- function(x, ...) {
    v <- x$getinverse()
    if(!is.null(v)) {
        message("getting cached matrix")
        return(v)
    }
    data <- x$get()
    v <- solve(data, ...)
    x$setinverse(v)
    v ## Return a matrix that is the inverse of 'x'
}
