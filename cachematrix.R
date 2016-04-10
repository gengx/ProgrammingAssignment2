## The makeCacheMatrix() creates a special matrix that can store a cached copy of the inverse of the given matrixfunction, and the cacheSolve() function calcultes the inverse of the special matrix. It checks first if a cached copy exists. If so, it skips the calculation and returns the cached copy. Otherwise, calculate the inverse of the matrix and store the value in the special matrix.


## Create a special matrix that can store a cached copy of the inverse of the given matrix

makeCacheMatrix <- function(x = matrix()) {
	 i <- NULL
	 set <- function(y) {
                x <<- y
                i <<- NULL
        }
     get <- function() x
     setinverse <- function(inverse) i <<- inverse
     getinverse <- function() i
     list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


## Calculate the inverse of the special matrix created by the makeCacheMatrix() function. Check first if a cached copy exists. If so, skip the calculation and return the cached copy. Otherwise, calculate the inverse of the matrix and store the value in the special matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
