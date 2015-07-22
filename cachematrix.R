## The two functions below (makeCacheMatrix and cacheSolve 
##	cache the inverse of a matrix  for future use. 
##It is assumed that the input matrix is invertible.

## The function makeCaheMatrix takes as an input a matrix
## and returns a list of 4 other functions:
##     - set: sets the matrix, takes a matrix as argument
##     - get: gets the matrix
##     - setinv: sets the inverse, takes a matrix (inverse) as argument
##     - getinv: gets the inverse

makeCacheMatrix <- function(m = matrix()) {
    ## First we set the inverse to NULL
    inv <- NULL
    ## We now define the first function, set. The function looks in
    ## the parent directory for a variable y and makes the matrix = y
    set <- function(y) {
    	m <<- y
    	inv <<- NULL
    }
    ## The second function, get, gets the matrix
    get <- function() {
    	m
    }
    ## The third function, setinv, sets the inverse equal to its
    ## argument. We will use this function to cache the result
    setinv <- function (newinv) {
    	inv <<- newinv
    }
    ## Finally, getinv, gets the inverse
    getinv <- function () {
    	inv
    }
    ## The function makeCacheMatrix returns a list of the 4 functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function cacheSolve takes as an argument the list of functions
## returned by the function makeCacheMatrix.
## First, it checks if an inverse exists in 
## memory. If so it fetches it. Otherwise it calculates the inverse
## of the input matrix and caches the result

cacheSolve <- function(flist, ...) {
    inv <- flist$getmean()
    ## First, it checks to see if an inverse already exists. If so we
    ## return it
    if(!is.null(inv)) {
    	message("getting cached data...")
    	return(inv)
    }
    ## If the inverse does not exist, we calculate it and cache it
    mat <- flist$get()
    inv <- solve
    flist$setmean(inv)
    inv
}
