##
## Sample interaction with this program:
##
## > i <- matrix(rnorm(16), 4, 4);
## > m <- makeCacheMatrix(i);
## > cacheSolve(m); ## This will compute fresh
## > cacheSolve(m); ## This will return cached value of matrix inverse
##

##
## Create a matrix and associate various get/set functions for data and its inverse
##
makeCacheMatrix <- function(x = matrix()) {

    inverse <- NULL
    set <- function(y) {
        x <<- y;
        inverse <<- NULL;
    }
    get <- function() x;
    setinverse <- function(inv) inverse <<- inv;
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##
## Compute the inverse of provided matrix. If inverse is already cahced, return cahced value.
##
cacheSolve <- function(x, ...) {

    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }

    data <- x$get()
    inv <- solve(data);
    x$setinverse(inv);
    inv
}

