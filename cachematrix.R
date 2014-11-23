## Creating a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    
    ## Setting the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## Getting the matrix
    get <- function() x
    
    
    ## Setting the inverse of the matrix
    setinverse <- function(solve) i <<- solve

    
    ## Getting the inverse of the matrix
    getinverse <- function() i
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    
    ## Return the inverse if its already set
    if( !is.null(i) ) {
        message("getting cached data")
        return(i)
    }
    
    ## Getting the matrix 
    data <- x$get()
    
    ## Calculating the inverse using matrix multiplication
    i <- solve(data, ...)
    
    ## Setting the inverse
    x$setinverse(i)
    
    ## Return the matrix
    i
}