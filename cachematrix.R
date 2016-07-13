## The first function, makeCacheMatrix creates a "matrix", which is really a 
## list containing a function to
## 1. Set the value of the matrix.
## 2. Get the value of the matrix.
## 3. Set the inverse of the matrix.
## 4. Get the inverse of the matrix.
makeCacheMatrix <- function(x = matrix())
{
    m <- NULL
    set<-function(y = matrix())
    {
        x <<- y
        m <<- NULL
    }
    get <- function()x
    setInverse <- function(inv)m <<- inv
    getInverse <- function()m
    list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)
}


## The following function calculates the inverse of the "matrix" created with
## the above function. It first checks if the inverse has already been
## calculated. If so, it gets the inverse from the cache and skips
## computation. Otherwise, it calculates the inverse of the matrix and sets
## the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m))
    {
        message("Getting cached data.")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInverse(m, ...)
    m
}
