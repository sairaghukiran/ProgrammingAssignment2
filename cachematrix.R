## The following functions are designed to calculate the matrix inverse of a given matrix.
## If the inverse of the object has been calculated previously, the function returns the matrix inverse 
## from a cached copy, avoiding expensive calculations.

## This function takes in a matrix, and returns a list of functions to set, get the value of the matrix and also 
## to set and get the value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() {
				x
		}
        setMatrixInverse <- function(matrixInverse) {
				m <<- matrixInverse
		}
        getMatrixInverse <- function() {
				m
		}
        list(set = set, get = get,
             setMatrixInverse = setMatrixInverse,
             getMatrixInverse = getMatrixInverse)

}


## This function takes the output from the above function as the input and checks if the matrix inverse has already been calculated.
## If it has been calculated, then it returns a cached copy of the result, else it computes and returns the result.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getMatrixInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setMatrixInverse(m)
        m

}
