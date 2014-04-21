## A wrapper Matrix cache which stores the inverse of a matrix

## This function creates a wrapper object for matrix which caches the
## matrix inverse so that it does not have to be calculated again and again

makeCacheMatrix <- function(x = matrix()) {
    inverseX <- NULL
    set <- function(y) {
        x <<- y
        inverseX <<- NULL
    }
    
    get <- function() {
        x
    }
   
    setInverse <- function(inv) {
        inverseX <<- inv
    }

    getInverse <- function() {
        inverseX
    }

    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## This function should be used to retrieve the inverse of the matrix
## If inverse has previously been calculated it fetches that and returns it
## If not previously calculated it calculates it and caches it by calling
## setInverse on the matrix cache object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseX <- x$getInverse()
        if (!is.null(inverseX)) {
            message("got cache data")
            return(inverseX)
        }

        data <- x$get()
        inverseX <- solve(data)
        x$setInverse(inverseX)
        return(inverseX)
}
