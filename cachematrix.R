## The following function computes the inverse matrix.
## It first checks to see if the inverse matrix has already been computed. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it computes the inverse matrix and caches it.



## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)

}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmean(m)
        m
}

## Below are testing code
## x<-matrix(c(4,3,3,2),nrow=2,ncol=2)
## makeCacheMatrix(x)
## cacheSolve(makeCacheMatrix(x))
