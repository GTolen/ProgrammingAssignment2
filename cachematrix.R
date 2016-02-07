## Assignment 2: Lexical Scoping
## 

## makeCacheMatrix function creates a special matrix
## object that can cache its inverse
## This returns a list of function:
## 1) set: set the value of the matrix        
## 2) get: get the value of the matrix
## 3) setInverseMat: set the value of the inverse of the matrix
## 4) getInverseMat: get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        test <- NULL
        set <- function(y) {
                x <<- y
                test <<- NULL
        } 
        get <- function() x
        setInverseMat <- function(InvMat) test<<- InvMat
        getInverseMat <- function() test
        list(set = set,
             get = get,
             setInverseMat = setInverseMat,
             getInverseMat = getInverseMat)
}


## cacheSolve computes the inverse of the matrix from
## the makeCacheMatrix function above.  If the inverse has already
## been calculated (and the matrix has not changed), cacheSolve will
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        test <- x$getInverseMat()
        if(!is.null(test)) {
                message("getting cached data")
                return(test)
        }
        TargetMat <- x$get()
        test <- solve(TargetMat, ...)
        x$setInverseMat(test)
        test
        
}
