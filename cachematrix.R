## Programming with R Assignment 2
## These 2 functions below cache the invesre of a matrix

## This function creates a 'special' matrix that is the inverse of the matrix passed in.
## This matrix is really a list that contains functions to:
## set the value of the inverse matrix
## get the value of the inverse matrix
## get the value of the mean 
## set the value of the mean 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- solve(y)  ## assume matrix is invertible and return its inverse
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}


## This function take a 'special matrix' (a list) created by makeCacheMAtrix. 
## If the inverse of this matrix is cached it returns this value else the inverse matrix is created and 
## the mean returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    inverseData <- solve(data)      ## data is matrix, solve finds its inverse
    m <- mean(inverseData, ...)
    x$setmean(m)
    m
}
