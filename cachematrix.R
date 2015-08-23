## Put comments here that give an overall description of what your
## functions do

## The function below should create a special matrix to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <-- NULL
        }
        #function to set matrix
        get <- function() x
        #function to get matrix
        setinverse <- function(inverse) m <<- inverse
        #function to set inverse of x
        getinverse <- function() m
        #function to get inverse of x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function will compute the inverse of the a matrix returned by makeCacheMatrix. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #if function checks if inverse of matrix exists, if it does, it returns cached inverse
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
