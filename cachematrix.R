## Functions for inverting matrixes, which uses cashed inverse matrix, if input matrix doesn't change

## Set and get input matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Calculate the inverse, checking if it was already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}


## Check

k<-matrix(c(1, 4, 6, 8, 9, 6, 1, 7, 5), nrow = 3, ncol = 3)
z<-makeCacheMatrix(k)
cacheSolve(z)
cacheSolve(z) ## Once again
