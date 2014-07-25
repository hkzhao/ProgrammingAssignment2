## Functions makeCacheMatrix and cacheSolve implement a special kind of 
## matrix that can cache its inverse, so that the inverse computation on 
## this matrix is done just once. To use it, the matrix has to be wrapped 
## by using makeCacheMatrix. Then, you can call cacheSolve on the wrapped
## matrix to compute/retrieve its inverse.

## Function makeCacheMatrix wraps a matrix, so that its inverse can be 
## cached. It returns the wrapped matrix. Call cacheSolve on the wrapped 
## matrix to compute/retrieve its inverse
## Note that the original matrix has to be invertible
makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	
	## set the original matrix x, reset cached inverse
	set <- function(y) {
		x <<- y
		s <<- NULL
	}
	
	## get the original matrix x.
	get <- function() x
	
	## set the cached inverse
	setsolve <- function(sv) s <<- sv
	
	## return the cached inverse
	getsolve <- function() s
	
	## return a list of functions for setting/retrieving original matrix
	## and the cached inverse
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Function cacheSolve takes wrapped matrix returned by function 
## makeCacheMatrix. If there is a cached inverse, it will just return it 
## without computing. However, if the cached inverse does exist, which 
## happens when you first call this function on a wrapped matrix, it 
## will compute it and then cache it. 
cacheSolve <- function(x, ...) {
    
	## check if inverse is cached
	s <- x$getsolve()
	if (! is.null(s)) {
		## found cached inverse, just return it
		message("getting cached data")
		return(s)
	}
	
	## cached inverse not found, compute it
	data <- x$get()
	sv <- solve(data, ...)
	## cache the computed inverse
	x$setsolve(sv)
	
	## Return a matrix that is the inverse of 'x'
	sv
}
