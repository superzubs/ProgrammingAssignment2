## Programming Assingment Week 3

## Function makeCacheMatrix
## Objective: creates a special "matrix" object that can cache its inverse.
## Input: x which is a matrix (assume that it is invertible)
## Output: A list containing four functions to set and get the value of matrix and its inverse respectively

makeCacheMatrix <- function(x = matrix()) {
  mx <- NULL
  # mx is set to NULL to clear previous value
  # declaring it as an object within the makeCacheMatrix environment
  set <- function(y) {
    x <<- y      # setting the value
    mx <<- NULL  # clears previous cache if any
  }
  # Function to get the value of matrix
  get <- function() x
  # Function to set the inverse when there is no cached inverse
  setinv <- function(inverse) mx <<- inverse
  # Function to get the inverse
  getinv <- function() mx
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Function cacheSolve
## Objective: calculates the inverse of matrix from makeCacheMatrix
##          : it will retrieve calculated inverse if the matrix has not changed
## Input: x is a list which is an output of makeCacheMatrix that contains 4 set of functions
## Output: An inversed matrix of x

cacheSolve <- function(x, ...) {
  # Get the cached value of the inversed matrix
  mx <- x$getinv()
  # If there is no value, then the matrix needs to be inversed
  if(!is.null(mx)) {
    message("getting cached data")
    return(mx)
  }
  # Get the matrix value
  data <- x$get()
  # Using base::solve to inverse the value
  mx <- solve(data, ...)
  # Cache the inversed value
  x$setinv(mx)
  # Returning the value
  return(mx)
}

# Testing
testmatrix <- matrix(c(5,3,2,9),2,2)

a = makeCacheMatrix(testmatrix)

cacheSolve(a)
