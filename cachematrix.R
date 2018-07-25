## Put comments here that give an overall description of what your
## functions do

##Returns a matrix with its index cache as a list of functions

makeCacheMatrix <- function(matrix = matrix()) 
{
    inv <- NULL
    set <- function(y)
    {
        matrix <<- y
        inv <<- NULL
    }
    get <- function() matrix
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    
    list(set = set, get = get,
         setInverse = setInverse, getInverse = getInverse)
}


##Returns the inverse of matrix x from cache(if available)
##      or calculates the inverse(if not available)

cacheSolve <- function(x, ...)
{
    inverse <- x$getInverse()
    
    if(!is.null(inverse))
    {
        message("Getting cached data...")
        return(inverse)
    }
    
    inverse <- solve(x$get(), ...)
    x$setInverse(inverse)
    inverse
}
