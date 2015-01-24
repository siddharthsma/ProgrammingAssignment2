## The 2 methods work together. First the makeCacheMatrix is passed a matrix,
## it returns a list with function elements. The cacheSolve method can then 
## be called passing that list as an argument. It will then return the inverse
## either by calculating it or by retrieving it from the cache. 

## This function takes an oridinary matrix as an argument and retruns
## a list of 4 elements, each of which as functions. The 'set' function
## simply sets the internal value of the variable x to the matrix. The'
## 'get' function returns the matrix x. 'setinverse' sets the matrix inverse,
## and 'getinverse' returns the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse <- NULL
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matrix_inverse <<- inverse
  getinverse <- function() matrix_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Given a cached matrix (which is really just the list returned from the
## makeCacheMatrix function), this function first tries to get the inverse.
## If x$getinverse is NULL it means the matrix inverse was not cached, and 
## so the function proceeds to get the actual matrix out of the cached matrix
## and calculate the inverse using the solve function. The inverse is then "cached"
## by calling the setinverse method and passing the inverse. The inverse is then
## returned. Alternatively if the inverse was cached, getinverse would return the
## inverse (not NULL), and that would be returned along with a message indicating
## that the value was returned from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrix_inverse <- x$getinverse()
  if(!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
  }
  data <- x$get()
  matrix_inverse <- solve(data, ...)
  x$setinverse(matrix_inverse)
  matrix_inverse
}
