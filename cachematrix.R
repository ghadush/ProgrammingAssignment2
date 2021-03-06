## This function uses to catch an inverse of matrix to seve time and in recalucaltion
## when the compuation is large size of matrix array and reuse the inverse of the matrix assuming it is square matrix

## The new concept intrdouced here is also "<<-" for global use of varibales

## Functions that cache the inverse of a matrix
##
## Usage example:
##
## > source('cachematrix.R')
## > m <- makeCacheMatrix(matrix(c(0, 1, 1, 0), c(2, 2)))
## > cacheSolve(m)
## [,1] [,2]
## [1,]    0    1
## [2,]    1    0

## Create a special "matrix", which is a list containing
## a function to
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks if the inverse result is already there and the matrix not changed
## if it is already there and stored in the cache it returns and exits the function
## if the cathc is null then it calculates by calling the get() makeCacheMatrix

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
