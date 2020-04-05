## These are a couple of functions that 1. take a matrix and 2. calculate and cache the inverse of the matrix
## If the inverse has already been calculated, the second function gets the "cached" inverse and returns that instead of calculating it


## This creates a list where the list contains functions and a couple of objects that can hold the matrix and its inverse
## for each of the functions, the environment is the list and they are able to access the objects that hold the matrix and its inverse
## because that's the environment they were created in.
## you can set the value of the matrix (x) in calling makeCacheMatrix or by using the set function within

makeCacheMatrix <- function(x = matrix()) {
  invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinv <- function(solve) invmat <<- solve
  getinv <- function() invmat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This takes as an argument a list created via makeCacheMatrix, uses the makeCacheMatrix$getinv function to test whether the inverse of the matrix
## has been calculated or not. If it has (i.e. getinv doesn't return NULL), it returns the already saved inverse.
## Otherwise, it calculates the inverse and uses the setinv function to store/cache it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmat <- x$getinv()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinv(invmat)
  invmat
}
