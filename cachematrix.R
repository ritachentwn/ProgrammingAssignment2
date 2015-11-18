###############################################################################
# Write a pair of functions that cache the inverse of a matrix.
###############################################################################
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverseX <- NULL
        get <- function() x   #get the value of the matrix
        setMatrix <- function(inverseMatrix)  inverseX <<- inverseMatrix  #set the value of the mean
        getMatrix <- function() inverseX   #get the value of the mean
        list1 <- list(get = get,   
                      setMatrix = setMatrix,
                      getMatrix = getMatrix)
}

## Return a matrix that is the inverse of 'x'
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        inverseX <- x$getMatrix()
        if(!is.null(inverseX)) {
                message("getting cached data")
                return(inverseX)
        }
        data <- x$get()
        inverseX <- solve(data)
        x$setMatrix(inverseX)
        inverseX
}


mm <- makeCacheMatrix(matrix(c(1:4), nrow = 2, byrow = TRUE))
cacheSolve(mm)
