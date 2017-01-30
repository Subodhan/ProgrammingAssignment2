## makeCacheMatrix function will create a special matrix, which will have functions to get the data (get),
## set the matrix data (set), set the inverse of the matrix (setInverse) or get the cached inverse matrix (getInverse)
##te 
##
## cacheSolve will take the "special" matrix as input (which has been created using the above function), and then
## use the functions of these special matrix to determine if the Inverse has already been calculated. If yes, then 
## the cached value is returned. If not, then the inverse is calculated and saved using the setInverse function for future use.

## makeCacheMatrix function will take a matrix as input and return a special matrix which has four functions as below:
## get - returns th stored matrix data
## set - saves the matrix data into a separate environment
## getInverse - returns the saved inverse matrix from the cache in separate environment
## setInverse - saves the supplied Inverse matrix in the cache environment

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve function will ake as input the "special" matrix
## The function will first check (using the functions of the input matrix) if the matrix already has a Inverse
## solution. If yes, it will returned the inverse matrix from the cache, else it will calculate the 
## inverse matrix, save it in the cache and return it.

cacheSolve <- function(x, ...) {
        ## check if the inverse is already available
		## If yes, return the value from cache
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		
		## else, get the data, calculate the inverse, save it in teh cache and return the result 
		## that is the inverse of 'x'
        data <- x$get()
        m <- solve(data,...)
        x$setInverse(m)
        m
}
