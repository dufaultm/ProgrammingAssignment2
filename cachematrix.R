## Title: cachematrix.R
## ProgrammingAssignment2
## Coursera 2016

##The "makeCacheMatrix" function creates a special "Matrix", which is really a list containing a function to:
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse matrix
##4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set.inv <- function(solve) m <<- solve
  get.inv <- function() m
  list(set = set,
       get = get,
       set.inv = set.inv,
       get.inv = get.inv
  )
}

## Calculates the inverse of the special "Matrix" created by makeCacheMatrix(). However, it first checks
## to see if the inverse has already been calculated. If so, it retrieves the cached inverse matrix and skips
##the computation. Otherwise, it calculates the inverse matrix inverse matrix using the solve() function
## and sets the solve value in the cache via the set.inv() function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get.inv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$set.inv(m)
  m
}
