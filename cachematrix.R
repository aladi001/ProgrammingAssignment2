## These functions cashe the inverse of a matrix

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
           i <- NULL
    set <- function(y){
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setInverse <- function(solveMatrix) i <<- solveMatrix
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  ## This now return a matrix that is the inverse of x
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i      
}

