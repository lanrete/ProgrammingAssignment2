## These functions can cache the inverse of a matrix rather than compute it repeatedly
## Since the computation of matrix inversion is usually costly in time

## makeCacheMatrix : 
## creates a matrix object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function(){x}
    setinverse <- function(inversed=matrix()) {m <<- inversed}
    getinverse <- function() {m}
    list (set = set,
          get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## cacheSolve :
## if (the inverse has not been calculated)
##    solve the inverse with the built-in function 'solve(x)' 
##    return the inverse of the matrix
## if (the inverse is already calculated)
##    this function retrieved the inverse instead of computing the result once again

cacheSolve <- function(x = matrix(), ...) {
    m <- x$getinverse()
    if (!is.null(m)){
        message("Getting Cached Data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
