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
	inverse <- NULL

	#
	# store the original matrix
	#
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}

	#
	# return the original matrix
	#
	get <- function() x

	#
	# store the inverse
	#
	setInverse <- function(i) inverse <<- i

	#
	# return the inverse
	#
	getInverse <- function() inverse

	#
	# return the special matrix object
	#
	list(set = set, get = get, setInverse = setInverse, 
		getInverse = getInverse)
}


#
# Computes the inverse of the special matrix returned by makeCacheMatrix.
#
# input parameter: x, the matrix whose inverse is needed
# returns:         a matrix containing the inverse
#
cacheSolve <- function(x, ...) {

	#
	# attempt to get cached inverse
	#
	inverse <- x$getInverse()
	if (!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}

	#
	# inverse was not cached; create it now
	#
	data <- x$get()
	inverse <- solve(data)
	x$setInverse(inverse)

	#
	# return the solution
	#
	inverse
}

