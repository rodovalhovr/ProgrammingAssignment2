# This function creates a special "matrix" object
# that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # set the value of the matrix
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # get the value of the matrix
        get <- function() x
        # set the value of the inverse matrix
        setInv <- function(inv_matrix) {
                m <<- inv_matrix
        }
        # get the value of the inverse matrix
        getInv <- function() m
        # return special matrix (list of functions)
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

# Computes the inverse of the special "matrix" returned
# by makeCacheMatrix. If the inverse has already been 
# calculated, then it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        # Checks if the inverse has already been calculated.
        # If so, it gets the inverse from the cache. 
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # Otherwise, it calculates the inverse of the matrix
        data <- x$get()
        m <- solve(data, ...)
        # and sets the value of the inverse in the cache
        x$setInv(m)
        # then, it returns a matrix that is the inverse of 'x'
        m
}
