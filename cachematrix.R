## These functions enable to cache the inverse of a matrix

## Create a CacheMatrix with list of get, set, setinverse, and getinverse functions

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
             setinverse = setinverse, getinverse = getinverse)
}


## Cache the inverse of the matrix

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
