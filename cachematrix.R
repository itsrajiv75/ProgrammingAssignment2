## Set of functions to create an inverse of a matrix and caches it.

## makeCacheMatrix function creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## Variable to store the cached inverse value, initialized to NULL
        cachedMatInverse <- NULL
        
        ## To create and set the matrix in the working environment
        set <- function(y = matrix()) {
                x <<- y
                cachedMatInverse <<- NULL
        }
        
        ## To get the value of the matrix
        get <- function() x
        
        ## Invert the matrix and store in cache
        setInverse <- function(matInverse) cachedMatInverse <<- matInverse
        
        ## Get the inverted matrix from cache
        getInverse <- function() cachedMatInverse
        
        ## Return the created functions to the working environment
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve function computes the inverse of the matrix.
## If the inverse has already been calculated then it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Get the inverse of the matrix stored in cache
        matInverse <- x$getInverse()
        
        ## Return inverse of matrix from cache if it exists
        ## else create the matrix in working environment
        if (!is.null(matInverse)) {
                message("Getting cached inverse of matrix")
                
                # Return cached inverted matrix
                return(matInverse)
        }
        
        ## Create matrix object
        matrixObj <- x$get()
        
        ## Calculate inverse of matrix
        matInverse <- solve(matrixObj, ...)
        
        ## Set inverted matrix in cache
        x$setInverse(matInverse)
        
        ## Return inverted matrix
        return (matInverse)
}
