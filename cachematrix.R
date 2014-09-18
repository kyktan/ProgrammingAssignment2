## Below are two functions that are used to 1) create a special object 
## that stores a matrix and caches the inverse of a matrix,
## and 2) compute the inverse of the matrix. If the inverse 
## has been calculated and the matrix is unchanged, 
## then cacheSolve retrieves the inverse from the cache

## Creates a special "matrix" object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Gives the matrix to be inverted
    get <- function() x
    
    ## Set the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    ## Gives the inverse if cached
    getinverse <- function() inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of the matrix returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    
    ## If already calculated, return cached inverse
    if(!is.null(inv)) {  
        message("getting cached data")
        return(inv)
    }
    
    ## If inverse not calculated, then calculate and cache it
    data <- x$get()
    inv <- solve(data, ...)  ## Compute inverse
    x$setinverse(inv)  ## Cache inverse
    inv
}
