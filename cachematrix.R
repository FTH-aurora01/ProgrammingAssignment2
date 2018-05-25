## This pair of functions calculate the inverse of a matrix, given it's invertible. If this inverse was calculated before and is 
## stored in cache, the functions use this cached value instead of calculating it again, saving computational resources.

## This function creates a list containing a function to (i) set the value of the matrix (ii) get the value of the matrix
## (iii) set the value of the matrix inverse an (iv) get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- 
			if(dim(data)[1]==dim(data)[2] && det(data) != 0 ) {solve(data, ...)}
			else {"Matrix supplied is not invertible"}
        x$setsolve(m)
        m
}
