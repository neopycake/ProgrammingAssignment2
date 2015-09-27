## Put comments here that give an overall description of what your
## functions do

## The functions makeCacheMatrix and cacheSolve are used to calculate, store and
## return an inverse of a matrix x. Calculation time is optimised by firstly
## checking if the inverse were already calculated and stored in the variable i.



## Write a short comment describing this function

## makeCacheMatrix is a function that contains and addresses different functions
## dealing with the task of calculating and storing of an inverse of a matrix x.
## The functions stored in makeCacheMatrix (set, get, setinv, getinv) can also
## be called by other functions e.g. cacheSolve.

## The function "set" is used to input a new matrix.
## The function "get" returns the latest matrix stored in "set".
## The function "setinv" stores the inverse of a matrix (or potentially any other
## values)
## The function "getinv" returns the values stored by "setinv".

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

## If the inverse i of the matrix x were already calculated and is stored in the 
## cache, the function returns the message "getting cached data" followed by i.
## Otherwise it calculates the inverse i, stores the solution and returns i.
## To store the inverse it calls the function "setinv" within makeCacheMatrix.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i  
}
