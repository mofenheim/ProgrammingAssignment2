## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function will return a list of available functions that will be later be invoked
#by the function cacheSolve
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  #sets the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #gets the matrix
  get <- function() x
  #sets the inverse of the matix
  setinv <- function(solve) inv <<- solve
  #gets the inverse of the matrix
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
#This function will invoke functions specified in makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  #If the inverse has already been calculated)
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #If not invokes the functions specified in makeCacheMatrix to read
  #in a matrix and return it's inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
