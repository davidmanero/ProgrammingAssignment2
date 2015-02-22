## In order to avoid complex computing calculations, this funtions calculate the
## inverse of a matrix and cache the results.

## makeCacheMatrix creates a special matrix object (in fact it is a list) to cache
## the inverse matrix of a matrix given. It contains a function to set the value 
## of the matrix, get the value of the matrix, set the value of the inverse matrix
## and get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # sets the value of m to NULL (it is a default if cacheSolve has not been used yet)
        y <- NULL # sets the value of y to NULL (it is a default if cacheSolve has not been used yet)
        set <- function(y) { # ser the value of the matrix
                x <<- y # caches the inputted matrix so that cacheSolve can check whether it has changed or not.
                m <<- NULL # sets the value of m (the inverse matrix if used cacheSolve) to NULL
        }
        get <- function() x # defines the get part of the list
        setinv <- function(solve) m <<- solve # defines the setinv part of the list (sets the inverse matrix)
        getinv <- function() m # defines the getinv part of the list (gets the inverse matrix if calculated)
        list(set = set, # creates the list of the 4 functions defined in this special "matrix"
             get = get,
             setinv = setinv,
             getinv = getinv)

}


## cacheSolve calculates the inverse of the special "matrix" created by 
## makeCacheMatrix. First it checks if the inverse has already been calculated. 
## If so, it get the inverse from the cache and skips calculation. If not, it
## use the function solve that gives the inverse of a matrix given. 

cacheSolve <- function(x, ...) {
        m <- x$getinv() # gets the inverse matrix if it has been already calculated
        if(!is.null(m)) { # check if cacheSolve has run before
                message("getting cached data")
                return(m) # if so, return the cached inverse matrix
        }
        data <- x$get() # if not calculated before, sets the original matrix in variable data
        m <- solve(data, ...) # uses the solve function to calculate the inverse matrix
        x$setinv(m) #update the cache inverse matrix by running setinv function
        m #returns the inverse
}
