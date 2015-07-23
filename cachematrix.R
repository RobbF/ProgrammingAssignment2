## Create two functions, a first function that can create a matrix object that can cache its inverse
## and a second function that computes the inverse of the above function -
## or if inverse has already been calculated (and matrix not changed), will retrieve the inverse from cache


## function to create matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(matrix) x <<- matrix
    getmatrix <- function() m
    list(set = set, get = get,
    setmatrix = setmatrix,
    getmatrix = getmatrix)
}


## function to compute inverse of matrix created by makecachematrix - or retrieve it from cache (if not changed)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
