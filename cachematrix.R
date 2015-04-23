## makeCacheMatrix is a function which returns a list of small functions
## used to perform matrix initialization and calculation of the inverse matrix
## The list can be seen by calling makeCacheMatrix()

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)


}

## If makeCacheMatrix function called with a matrix as an argument,
## we can compute an invesre calling its small functions as a list memebers.
## set assigns the inv NULL value, so every time source matrix changed,
## we need to perform computation. If inv value is not NULL, we use 
## cached value instead of computation.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matr <- x$get()
        inv <- solve(matr)
        x$setinv(inv)
        inv
}
## Example usage:
## data <- makeCacheMatrix(x=matrix(c(2:5),2,2))
## cacheSolve(data)
