## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix stores a Matrix and if calcultaed its invers aswell

makeCacheMatrix <- function(x = matrix()) {
Inv <- NULL
        set <- function(y) {
                ## Var: x holds the Matrix
                x <<- y
                ## Var: Inv holds (if calculated) the invers of x
                Inv <<- NULL
        }
        ## returns x
        get <- function() x
        ## set the inverse of x
        setInvers <- function(invers) Inv <<- invers
        ## returns the invers of x
        getInvers <- function() Inv
        list(set = set, get = get,
             setInvers = setInvers,
             getInvers = getInvers)
}



## cacheSolve returns a cached Invers of the Matrix if it has been previously calculated 
## otherwise will it calculate and return the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- x$getInvers()
        ## test if Invers is already calculated
        if(!is.null(Inv)) {
                message("getting cached data")
                ## return previously calculated invers
                return(Inv)
        }
        ## else calculate new and return inverse 
        data <- x$get()
        Inv <- solve(data, ...)
        x$setInvers(Inv)
        Inv
}
