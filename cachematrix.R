## Caching the Inverse of a Matrix
### The pair of functions below help to reduce potentially costly computation involved in matrix inversion. Caching the inverse of a matrix could reduce the computational cost of computing the inverse of a matrix repeatedly. The functions below assume that the matrix supplied is always invertible.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
			x <<- y
			m <<- NULL
	}
	get <- function() x
	setinverse <- function() m <<- solve(x)
	getinverse <- function() m
	list(set = set, get = get, 
		setinverse = setinverse, 
		getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by the makeCacheMatrix function. If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
        		message("getting cached data")
        		return(m)
        }
        data <- x$get()
        m1 <- solve(data, ...)
        m1
}
