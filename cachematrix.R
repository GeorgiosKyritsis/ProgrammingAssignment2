##It calculates the inverse of a matrix and cache 
##the result rather than compute it repeatedly

##The first function creates a list containing
##a function to set the matrix, get the matrix,
##set the inverse matrix and get the inverse matriix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get = function() x
        setInverse = function(solve) m <<- solve
        getInverse = function() m
        list(set = set, get = get, 
             setInverse = setInverse,
             getInverse = getInverse)
}

##The second function calculates the inverse matrix
##created with the above function. It first checks if 
##the inverse matrix has already been calculated. If so 
##it skips the computatation. Otherwise it calculates the 
##inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}