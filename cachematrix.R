## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # Start by setting the variable up with Null
        inv <- NULL
        
        # Make set function
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # Make get function
        get <- function() x
        
        # Make function to set a solution to inverted mat
        setinv <- function(solve) inv <<- solve
        
        # Make function to return the inverted (stored) matrix
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # Get the inverted matrix to see if it exists
        inv <- x$getinv()
        
        # if it exists, return it
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # If nothing was already stored for the inverse, set it
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        return(inv)
}
