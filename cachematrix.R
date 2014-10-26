## Programming assignment 2
## The goal of this second programming assignment is to 
## write an R function that is able to cache.  

## This function creates a special "matrix" object that can cache its inverse.n

makeCacheMatrix <- function(x = matrix()) {
   
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse 
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function return the matrix inverse of x

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv    
}
