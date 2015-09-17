## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly

## The makeCacheMatrix function creates a matrix object 
## that will store the results of its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
    	x <<- y
        inv <<- NULL
    }
	get <- function() x
	setInv <- function(inverse) inv <<- inverse
	getInv <- function() inv
    list(set = set, get = get, setInv = setInv,
    	getInv = getInv)
}


## The cacheSolve function computes the inverse of the matrix created
## through the makeCacheMatrix function. The function will also test
## to see if the matrix exists.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        	inv <- x$getInv()
    if (!is.null(inv)) {
        message("Retrieving cached data ... please wait")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInv(inv)
    inv
}
