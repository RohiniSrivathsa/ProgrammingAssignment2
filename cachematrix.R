## Put comments here that give an overall description of what your
## functions do
##
## makeCacheMatrix function creates a special "matrix" object that 
## can cache its inverse.
##
makeCacheMatrix <- function(X = matrix()) {
    M <- NULL
    set <- function(Y) {
        X <<- Y
        M <<- NULL
    }
    get <- function() X
    setSolve <- function(SOL) M <<- SOL
    getSolve <- function() M
    list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}
##
## cacheSolve function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## ***(and the matrix has not changed)***, then cacheSolve retrieves 
## the inverse from the cache.
##
cacheSolve <- function(X, ...) {
    ## Return a matrix that is the inverse of 'x'
    InvX <- X$getSolve()
    if(!is.null(InvX)) {
        message("Getting cached data")
        return(InvX)
    }
    D <- X$get()
    InvX <- solve(D, ...)
    X$setSolve(InvX)
    InvX
}