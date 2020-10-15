## Builds a custom matrix object that contains its inverse. Prevents repeated
## calculation of matrix inversion by storing the inverted matrix.

## Initializes matrix, retrieving a matrix 'y' from global env. Sets the inverse
## to NULL. Functions are stored in a list, so when the matrix is called by
## list object it performs the function. Get_inverse and set_inverse retrieve
## and store the inverted matrix in the cache, respectively. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Checks for existing inverted matrix. If not found, then retrieves the matrix,
## creates an inverted matrix, sets it within 'x', and returns the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$get_inverse()
    if(!is.null(inverse)) {
        message("Retrieving cached inverted matrix.")
        return(inverse)
    }
    mat <- x$get()
    inv <- solve(x, ...)
    x$set_inverse(inv)
    inv
}
