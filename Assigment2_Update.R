#Function set the cache

makeCacheMatrix <- function (x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)    
        
}
# function that calculates the inverse of matrix x 

cacheSolve <- function (x, ...) {
        
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
                }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        
}