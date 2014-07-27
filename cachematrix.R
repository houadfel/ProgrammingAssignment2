## cachematrix.R
##
## caches the inverse of a matrix thus eliminating the need to calculate 
## the inverse more than once for multiple uses. At first use, the inverse is 
## actually calculated, then cached in. At subsequent uses, the inverse is only 
## recalled from the cache and used. The method combines two functions:
## - makeCacheMatrix() to store the cache matrix, and,
## - cacheSolve() to return the inverse of the matrix.
##
## Usage:
## x <- matrix(c(3,1,-1,1,4,-1,1,-1,9),3,3)
## y <- makeCacheMatrix(x)
## xinv <- cacheSolve(y)


##
## makeCacheMatrix() creates the cache matrix from a regular matrix.
##
##      Input: a matrix x,
##
##      Output: : a cache matrix which is a list of four functions:
##
##      set(): sets the value of a matrix
##      get(): gets the value of a matrix
##      setinverse(): sets the value of a matrix inverse
##      getinverse(): gets the value of a matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Return a cache matrix which is an object consisting of a list
        ## of four functions: set(), get(), setinverse(), getinverse()
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve() returns the inverse of a matrix that have been previously 
## cached using makeCacheMatrix(). The function operates as follows: 
## 1. It checks to see if the inverse has already been calculated. 
## 2. If so, it gets the inverse from the cache and skips the computation. 
## 3. Otherwise, It calculates the inverse and sets it in the cache.
## 
##      Input: an object of type cache matrix (returned by makeCacheMatrix())
##
##      Output: Inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setinv(inv)
        inv
}
