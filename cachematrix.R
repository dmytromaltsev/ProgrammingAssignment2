## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix and caches its inverse  if used in combination with next function

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

## This function calculates the inverse of the matrix returned by the function above. If inverse of the same matrix is asked more than once subsequently, then no recalculations are made, the already calculated cached value is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

z <- matrix(c(1, 4, 6, 4, 2, 7, 8, 9, 1), 3, 3)
cacheSolve(makeCacheMatrix(z))
