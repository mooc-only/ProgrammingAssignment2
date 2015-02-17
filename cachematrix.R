## Thanks to lexical scope feature and list subsetting operator it is able to 
## simulate an "object" with internal memory and methods

## makeCacheMatrix is a matrix's constructor, resembling the factory pattern
## used in object oriented programming paradigm. 
## It takes a conventional R's matrix and return a "cached matrix" object

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(m) {
        x <<- m
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve is a function wrapper for solve() function used in "cached matrix"
## object. 
## WARNING: The simplicity of the statement let some bugs to happen. 
## e.g.: x$setinverse( some_matrix  ) shouldn't be able to be called from any 
## context, it is needed something like 'protected' methods

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    m <- solve(x$get(), ...)
    x$setinverse( m )
    m
}
