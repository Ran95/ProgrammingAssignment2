## Thanks very much to Pavel Kirjanas's post on the forum. I haven't had a 
## programming course before, but now I understand what 'envrionment' means.
##----------------------------------------------------------------------------
## These two functions calculate the inverse of matrix 'x' and cache the 
## result. So when the same 'x' is to be calculated, the result is returned 
## directly from cache and time is saved.


## This function takes a matrix 'x' and returns a list of four other functions
## not directly related to the value of 'x'. The four functions are called 
## later in another environment, and they can refer to matirx 'x' in this 
## environment.

makeCacheMatrix <- function(x = matrix()) {

	m <- NULL	# 'm' will represent our cache (and in other environments as 
			# well). Every time 'makeCacheMatrix' is called (either 
			# with a different 'x' or not), the cache 'm' is cleaned.
	
	set <- function(y = matrix()) {	
			# If 'y' is a matrix and $set(y) is called(in another 
			# environment),'y' will replace matrix 'x' in this 
			# environment and the cache 'm' will be cleaned. "<<-" 
			# allows 'x' in this environment be changed even though 
			# the function is called in another environment. This 
			# function isn't used in our example. 
		x <<- y
		m <<- NULL
	}
	get <- function() { x } # This function returns matrix 'x' for another, 
					# namely the parent environment.

	setinv <- function(solve) { m <<- solve } 
			# This function will be called in the parent environment and
			# assign the inverse of 'x' to 'm' in this environment.

	getinv <- function() { m } # Returns 'm' in this environment for the
					   # parent environment.

	list(set = set, get = get, setinv = setinv, getinv = getinv)
}
## The output is a list of the functions.
## An important thing to notice is that when the four/three functions are 
## called later, 'm <- NULL' isn't executed again.


##----------------------------------------------------------------------------
## This function takes the output of 'makeCacheMatrix' function and actually 
## computes the inverse of matrix 'x' and caches the result.
cacheSolve <- function(x, ...) { # This 'x' is not the matrix 'x' but this 
					   # function's own input (the list). Because 
					   # it's in a different environment from the
					   # 'makeCacheMatrix' function. Letter 'x' can
					   # be used again.

	m <- x$getinv()	# This step assigns the 'm' in 'cacheSolve' the same 
			    	# value as the 'm' in 'makeCacheMatrix' (NULL for the
			    	# first time).

	if(!is.null(m)) { # Testing if the cache has a value or if 
				# 'makeCacheMatrix' was called again and thus cleaned 
				# the cache.
		message("getting cached data")
		return(m)	# 'return' the cached result directly.
	}

	data <- x$get()	# Retrieving matrix 'x' in 'makeCacheMatrix'.
	m <- solve(data, ...)	# Calculating the inverse and store in 'm' in
					# this environment.

	x$setinv(m)	# Then storing to 'm' in 'makeCacheMatrix' as well, because
			# otherwise 'm <- x$getinv()' would reassign the old value.

	m  ## Return a matrix that is the inverse of 'x'
}
