## This is a slight modification of the sample makeVector/cachemean function pair
## designed to cache matrices and, when calculated, their inverses

## makeCacheMatrix, when called creates a container object, 
## implemented as a list of four functions
## when created, the object essentially contians the provided matrix and 
## the value m is NULL serving as a flag that the inverse was not yet calculated
## when the cachesolve is first called with this object as an argument, 
## the inverse is calculated and stored in m.
## subsequent calls of cashesolve with this object as an argument 
## immediately return stored m

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() {x}
    
    setinv <- function(solved) {m <<- solved}
    
    getinv <- function() {m}
    
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}




## This a function works with objects created by makeCacheMatrix. Denote the argument mCM.
## The function first checks whether m, accessible as mCM$getinv, is null.
## if m is not null (i.e., it is the calculated inverse we need), it is returned 
## if m is null, the function gets the original matrix, calculates its inverse, 
## stores it in the environment of mCM using mCM$setinv, and returns this inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
    
}
