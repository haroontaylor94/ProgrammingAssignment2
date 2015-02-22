## The two functions create an object which stores a matrix and then caches' its inverse

## This function creates a list that contains 4 functions: set, get, setinverse & getinverse 
## That are able to cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get, 
             setinverse = setinverse, 
	     getinverse = getinverse)
}

## This function computes the inverse of a matrix returned by the makeCacheMatrix function
## If the inverse has previously been calculated it is retrieved from the cache
## Otherwise the inverse is calculated by the solve function
## The solved result is cached and then returned
cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
        ## Returns a matrix that is the inverse of 'x'
}
