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
##
## makeCacheMatrix()
## 
## This function is essentially a class definition.  A specific instance of
## this class is created by calling makeCacheMatrix() with an invertable
## matrix passed as the argument.  The result is a list containing four
## functions (or more technically 'methods' since we are talking OOP here).
##
## The four methods are: set(), get(), setInverse(), and getInverse().
##    set():  Stores a matrix in the instance. Used to change a stored matrix.
##    get():  Retrieves the original matrix from the instance.
##    setInverse():   Stores the inverse matrix in the instance.
##    getInverse():   Retrieves the cached inverse matrix from the instance.
##
makeCacheMatrix <- function(x = matrix()) {
    ## Creates member 'm' as a NULL object.
    ##
    m <- NULL
    
    ## The 'set' method allows us to change, after the fact, the matrix stored
    ## in the instance of makeChangeMatrix.
    ##
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## The 'get' method returns the original matrix 'x'.
    ##
    get <- function() x
    
    ## The 'setInverse' method stores the inverse matrix result in 'm'.
    ##
    setInverse <- function(solve) m <<- solve
    
    ## The'getInverse' method retrieves the inverse matrix stored in 'm'.
    ##
    getInverse <- function() m
    
    ## Return the list of functions.
    ##
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
##
## cacheSolve()
##
## This function is used to compute the inverse of a matrix and then
## cache the result for future retrieval rather than having to recompute
## the inverse all over again.
##
## The argument passed to cacheSolve() is an instance of makeCacheMatrix.
##
## The first time cacheSolve() is called it computes the inverse of the matrix
## with the solve() function, caches the result, and returns the inverse matrix
## to the user.  Subsequent calls to cacheSolve() will return the
## cached result.
##
cacheSolve <- function(x, ...) {
    ## Assigns to 'm' the inverse matrix result stored in the instance
    ## of makeChangeMatrix passed as an argument to the cacheSolve() function.
    ##
    m <- x$getInverse()
    
    ## If 'm' is not null this means you have already run the cacheSolve()
    ## function and can simply return return the inverse matrix value stored
    ## in 'm'.
    ##
    ## However, if the result of x$getInverse() returns NULL you have not
    ## yet calculated the inverse matrix and need to do so with the code
    ## follwoing the 'if' statement below.
    ##
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## Set the variable 'data' to the original matrix stored in the instance
    ## of makeChangeMatrix that was passed as an argument to the cacheSolve()
    ## function.
    ##
    data <- x$get()
    
    ## Generate the inverse of the matrix stored in 'data' using the solve()
    ## function, then store the result in 'm'.
    ##
    m <- solve(data, ...)
    
    ## Store 'm' in the instance of makeChangeMatrix that was passed as an
    ## argument to the cacheSolve() function.
    ##
    x$setInverse(m)
    
    ## Returns the inverse matrix result stored in 'm'.
    #
    m
}
