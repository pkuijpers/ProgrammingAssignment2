## These functions create a matrix that caches its inverse matrix. This to
## avoid costly recalculation of the inverse matrix.

## This function creates a special "matrix" object that can cache its inverse.
## It is a list of the functions
## set - set the value of the matrix
## get - get the value of the matrix
## setinverse - set the value of the inverse
## getinverse - get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
	     setinverse = setinverse, getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" created by
## makeCacheMatrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	matrix <- x$get()
	inv <- solve(matrix, ...)
	x$setinverse(inv)
	inv
}
