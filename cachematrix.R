##The first function, makeVector creates a special "matrix store", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the matrix inverse
##get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
	  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


## Write a short comment describing this function
## function to calculate inverse of special matrix with function above
## retrive stored inverse if exists; else calculate it

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s

}
