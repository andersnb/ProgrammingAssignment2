#
# A set of functions for managing a cache of a matrix inverse.
#
# modification history
# -------------------
# 13Nov2014  Bill Anderson    Created.
#

#
# Creates a "special" matrix object that can cache its inverse.
# 
# input parameter:   x, an optional matrix parameter 
# returns:           matrix object 
#
makeCacheMatrix <- function(x = matrix()) {
	solution <- NULL
	set <- function(y) {
		x <<- y
		solution <<- NULL
	}

	get <- function() x

	setSolution <- function(s) solution <<- s
	getSolution <- function() soluation
	list(set = set, get = get, setSolution = setSolution, 
		getSolution = getSolution)
}


#
# Computes the inverse of the special matrix returned by makeCacheMatrix.
#
# input parameter: x, the matrix whose inverse is needed
# returns:         a vector containing the solution
#
cacheSolve <- function(x, ...) {

	solution <- x$getSolution()
	if (!is.null(solution)) {
		message("getting cached data")
		return(solution)
	}

	data <- x$get()
	solution <- solve(data)
	x$setSolution(solution)
	data
}

