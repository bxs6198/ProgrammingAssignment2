## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix() and cacheSolve() must be used together in order to
## calculate an inverse matrix and cache the result for future retrieval.
##
## makeCacheMatrix() is a class definition that is passed an invertable
## matrix as it's argument.  A specific instance of this class is first
## instantiated, then passed as an argument to cacheSolve() which generates
## the inverse matrix and caches the result for future retrieval.


## Write a short comment describing this function

## makeCacheMatrix()
## 
## This function is essentially a class definition.  A specific instance of
## this class is created by calling makeCacheMatrix() with an invertable
## matrix as the argument.  The result is a list containing four functions.
##
## The four functions are: set(), get(), setInverse(), and getInverse().
##
##  set():  Stores the orignal matrix.
##  get():  Retrieves the original matrix.
##  setInverse():   Calls solve() with the stored matrix and caches result.
##  getInverse():   Retrieves the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

## cacheSolve()
##
## This function is used to compute the inverse of a matrix and then
## cache the result for future retrieval rather than having to recompute
## the inverse all over again.
##
## The first time cacheSolve() is called with an argument of the class
## makeCacheMatrix, it computes the inverse of the matrix with the
## solve() function, caches the result, and returns the inverse matrix
## to the user.  Subsequent calls to cacheSolve() will return the
## cached result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
