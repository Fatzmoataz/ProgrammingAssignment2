## The functions in this script allow to use caching in the computation of
## the inverse of a matrix

## the makeCacheMatrix function creates a special "matrix", which is 
## actually a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(computed_inverse) inverse <<- computed_inverse
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## the cacheSolve function computes the inverse of the special "matrix" created 
## with the above function. It first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Othewise, it computes the inverse of the data and sets the value of the inverse
## in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
