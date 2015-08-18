## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m, ...) {
    ms <- NULL
    set <- function(y) {
        m <<- y
        ms <<- NULL
    }
    get <- function() m
    setinverse <- function(inv) ms <<- inv
    getinverse <- function() ms
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    ## r1 <- c(0,1,4)
    ## r2 <- c(5,6,0)
    ## r3 <- c(3,2,5)
    ## m1 <- rbind (r1,r2,r3)
    ## m0 <- makeCacheMatrix(m1)
    ## m0$setinverse(solve(m1))
    ## m0$getinverse()
}


## Write a short comment describing this function

cacheSolve <- function(x,...) {
    mi <- x$getinverse()
    if(!is.null(mi)) {
        message("getting cached data")
        return(mi)
    }
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    i
}
