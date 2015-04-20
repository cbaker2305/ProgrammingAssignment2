## The two functions below cache and calculate the inverse of a given matrix

## The makeCacheMatrix creates a matrix object that can
## cache its inverse and store for later retrival

makeCacheMatrix <- function(mx = matrix()) {
        
        ##intialize inverse
        inverse <- NULL
        
        ##set matrix
        set <- function(y) {
                mx <<- y
                inverse <<- NULL
        }
        ##get matrix
        get <- function() return (mx)
        
        ##set inverse of matrix
        setinverse <- function(inv) inverse <<- inv
        
        ##get inverse of matrix
        getinverse <- function() return (inverse)
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function checks for the cached inverse of a matrix, and if not found
## find the inverse and caches it for future use

cacheSolve <- function(mx, ...) {
        
        ## Return a matrix that is the inverse of 'mx'
        ##gets inverse of matrix
        inverse <- mx$getinverse()
        
        ##if found in makeCacheMatrix, return inverse
        if(!is.null(inverse)) {
                message("getting cached data")
                return (inverse)
        }
        
        ##if inverse is not found, the inverse must be calculated
        ##return matrix
        data <- mx$get()
        
        ##Calculate inverse of matrix
        inverse <- solve(data, ...)
        
        ##Set the inverse of the matrix for later use
        mx$setinverse(inverse)
        
        ##return the inverse of the matrix
        return (inverse)
}
