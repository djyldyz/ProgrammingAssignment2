## Creating a special matrix that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
    
    i <- NULL
    
    ## Setting the matrix
    set <- function( matrix ) {
        m <<- matrix
        i <<- NULL
    }
    
    ## Getting the matrix
    get <- function() {
        ## Return the matrix
        m
    }
    
    ## Setting the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
    
    ## Getting the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    
    ## Return the inverse if its already set
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    
    ## Getting the matrix 
    data <- x$get()
    
    ## Calculating the inverse using matrix multiplication
    m <- solve(data) %*% data
    
    ## Setting the inverse
    x$setInverse(m)
    
    ## Return the matrix
    m
}