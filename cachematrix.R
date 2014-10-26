# Matrix inversion is usually a costly computation and their may be some benefit to 
# caching the inverse of a matrix rather than compute it repeatedly. 

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse. It creates a list that contains a function to:
#       1. set the value of the matrix
#       2. get the value of the matrix
#       3. set the value of inverse of the matrix
#       4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        varInv <- NULL
        set <- function(y) {
                x <<- y
                varInv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) varInv <<- inverse
        getinverse <- function() varInv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

# Note: the matrix has to be invertible
cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached inverse data.")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}
