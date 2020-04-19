## Function to cache a matrix and its inverse.
## Sorry about my english - I am from Brazil.

## The first (makeCacheMatrix) function just hold some matrix and provide functions
## to cache the inverse os this matrix. The first function also
## responsible to clean the cache if the original matrix change
##
## The second function (cacheSolve) get the cached result if is avaiable
## otherwise the function will cache the inverse of original matrix
## and return it

## makeCacheMatrix - creates an object that can hold a matrix
#  and also can cache the inverse os this matrix

makeCacheMatrix <- function(x = matrix()) {
    resultCache <- NULL
    
    set <- function(y) {
      x <<- y
      resultCache <<- NULL
    }
    
    get <- function() x
    setSolve <- function(solve) resultCache <<- solve
    getSolve <- function() resultCache
    
    list(set = set, get = get
         ,setSolve = setSolve
         ,getSolve = getSolve)
}


## Try to get the inverse matrix from cache of makeCacheMatrix.
## If the cache is not available then will 'solve' the original matrix
## and set result(cache) and return it

cacheSolve <- function(x, ...) {
    result <- x$getSolve()
    
    if(!is.null(result)) {
        return(result)
    }
    
    currentMatrix <- x$get()
    result <- solve(currentMatrix, ...)
    x$setSolve(result)
    
    result
}