## Create two functions call that will can call a matrix, create its inverse,
## and store it for later use if necessary in order to avoid redundant calculations

## 'makeCacheMatrix' creates a function that calls a matrix as its argument,
## creates an empty object (m) and uses 4 functions (set, get, setInverse, getInverse) to store and recall 
## the matrix

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) m <<-inverse
     getInverse <- function() m
     list(set=set, get=get, setInverse=setInverse, getInverse =getInverse )
}


## 'cacheSolve' computes the inverse of the matrix stored in the 'makeCacheMatrix' function.  If the inverse has 
## already been calculated then it will pull the inverse matrix from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getInverse()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	mat <- x$get()
	m <- solve(mat, ...)
	x$setInverse(m)
	m
}
