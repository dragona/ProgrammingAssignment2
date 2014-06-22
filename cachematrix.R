## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function

    # This is simply the implementation of the second programming assignment 
    # This second programming assignment will require you to write an R function is
    # able to cache potentially time-consuming computations. For example, taking the
    # mean of a numeric vector is typically a fast operation. However, for a very
    # long vector, it may take too long to compute the mean, especially if it has to
    # be computed repeatedly (e.g. in a loop). If the contents of a vector are not
    # changing, it may make sense to cache the value of the mean so that when we
    # need it again, it can be looked up in the cache rather than recomputed. In
    # this Programming Assignment will take advantage of the scoping rules of the R
    # language and how they can be manipulated to preserve state inside of an R
    # object.


makeCacheMatrix <- function( m = matrix() ) {
    
    ## Initialize the inverse property
    i <- NULL
    
    ## Method to set the matrix
    set <- function( matrix ) {
        m <<- matrix
        i <<- NULL
    }
    
    ## Method the get the matrix
    get <- function() {
        ## Return the matrix
        m
    }
    
    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }
    
    ## Method to get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }
    
    ## Return a list of the methods
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
    
    ## Just return the inverse if its already set
    if( !is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    
    ## Get the matrix from our object
    data <- x$get()
    
    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data
    
    ## Set the inverse to the object
    x$setInverse(m)
    
    ## Return the matrix
    m
}


