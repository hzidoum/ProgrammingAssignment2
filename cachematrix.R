# File: assignment2
# Caching the Inverse of a Matrix for future usage
# Date: 19 June 2014

# makeCacheMatrix: function creates a special "matrix" object that can cache 
# its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                # set default inverse matrix
    set <- function(y) {     # set the data matrix and default inverse matrix
        x <<- y
        m <<- NULL
    }
    get <- function() x      # return the data matrix
    setinverse <- function(solve) m <<- solve     #cache the inverse matrix
    getinverse <- function() m           # return the cached inverse matrix
    list(set = set, get = get,           # export a list of methods/functions
         setinverse = setinverse,
         getinverse = getinverse)
}

# cacheSolve: function returns a matrix that is the inverse of 'x'
# assumes that the matrix supplied x is always invertible

cacheSolve <- function(x, ...) {
    
    m <- x$getinverse()   #get the cached inverse matrix
    
    if(!is.null(m)) {                        # case where the inverse was previously
        message("getting cached data")       # cached
        return(m)
    }
    data <- x$get()         #case of de novo matrix      
                            #extract the value of the matrix data from the object x
    
    m <- solve(data)     #calculate the inverse matrix with built-in R function       
    
    x$setinverse(m)      #cache the newly computed inverse.
     
    m                    #return the inverse matrix
}

