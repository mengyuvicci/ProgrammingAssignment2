## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create set function to save the given Matrix to environment
## Create get function to retrieve Matrix 
## Create setInverse function to save the inversed Matrix to environment
## Create getInverse function to retrieve inversed Matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(invVal) inverse <<- invVal
    
    getInverse <- function() inverse
    
    list(set = set, get = get, 
         setInverse = setInverse,
         getInverse = getInverse)

}


## Write a short comment describing this function
## Check if the inversed Matrix of x is saved in the environment
## if so, then return the value
## if not, then inverse the Matrix by calling solve() function and 
## return the inversed Matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getInverse()
    if (!is.null(inverse)){
        message("getting cached matrix")
        return(inverse)
    }
    
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
    inverse
    
}
