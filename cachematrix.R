## Invert a matrix and then cache the result so that it doesn't need
## to be inverted each time.

## `makeCacheMatrix`: This function creates a special "matrix" object
##  that can cache its inverse. The matrix is actually a class that
##  includes a value and some functions (methods). The input is a 'regular'
##  matrix. Using the "CacheMatrix" member
##  functions, the CacheSolve function can take the regular matrix, invert it,
##  and save (cache) the inverted function back into the "matrix" object

makeCacheMatrix <- function(x = matrix()) {
        # new variable. This will be the solved matrix
        m <- NULL
        # function called 'set'. Takes in y, sets value of external x to y,
        # and then sets m back to null (? why ?)
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # function called 'get'. It reports back the value of x, the original
        # matrix
        get <- function() x

        # function called 'setinverse' It
        setinverse <- function(solved) m <<- solved

        # function called 'get'. It reports back the value of m, the solved
        # matrix
        getinverse <- function() m

        ## returning a list that includes the four values above
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x', where 'x' in this
        ## case is the special kind of matrix created by 'makeCacheMatrix'

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
