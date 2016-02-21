## These are two functions that are used to create a special 
## object that stores a numeric matrix and cache's the inverse 
## of this matrix.


## This first function, makeCasheMatrix creates a special 
## "matrix" which is really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverseMatrix <- function(solve) m <<- solve
  getinverseMatrix <- function() m
  list(set = set, get = get,
       setinverseMatrix = setinverseMatrix,
       getinverseMatrix = getinverseMatrix)
}


## This cacheSolve function calculates the inverse of the 
## special "matrix" created with the makeCacheMatrix 
## function.However, it first checks to see if the inverse of 
## matrix has already been calculated. If so, it gets the 
## inverse matrix from the cache and skips the 
## computation. Otherwise, it calculates the inverse matrix of 
## the data and sets the value of the inverse matrix in the 
## cache via the setinverseMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverseMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverseMatrix(m)
  m
}
