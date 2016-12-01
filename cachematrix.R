## Functions provide an ability to cache the inverse of a matrix
## and access it if it has been already calculated

## Creates a "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## This function computes the inverse of the special "matrix"
# or just gets the cached value if it has already been calculated

cacheSolve <- function(x, ...) {

  i <- x$getinverse()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  matrix <- x$get()  
  i <- solve(matrix, ...)
  x$setinverse(i)
  i
}


# Testing:

testmatrix <- matrix(rnorm(16), 4, 4)

cashedmatrix <- makeCacheMatrix(testmatrix)

cacheSolve(cashedmatrix)