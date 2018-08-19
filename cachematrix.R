## Clearing the previous R enviornment
rm(list=ls())

## This file will contain two functions: makeCacheMatrix and cacheSolve
## that will cache the inverse of a matrix fed into makeCacheMatrix.

## The makeCacheMatrix is the first function, which creates a special
## matrix object that will cache its inverse for a user inputted matrix.
## It is assumed that the matrix is an invertible square matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() 
    x
  
  setinv <- function(inverse) 
    m <<- inverse
  
  getinv <- function() 
    m
  
  list(set = set, 
       get = get, 
       setinv = setinv, 
       getinv = getinv)
}

## The cacheSolve function is the second function which computes the inverse
## of the matrix that was inputted into the makeCacheMatrix function above.
## If the invertible square matrix has already been calculated, then the
## cacheSolve function will output that. If the inverse of the matrix has not
## been calculated, then the cacheSolve function will calculate the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  
  if(!is.null(m)) {
    message("getting cached result")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setinv(m)
  
  m
}

## Testing the makeCacheMatric and cacheSolve functions written above.
test_matrix <- matrix(rnorm(9, 0, 1), 3, 3)
test_matrix

test_matrix.1 <- makeCacheMatrix(test_matrix)

cacheSolve(test_matrix.1)


