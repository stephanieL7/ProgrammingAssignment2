## Functions to solve a matrix, cache it's inverse, and return the cached inverse if the
  # same matrix object is returned through the second function. 

## This function creates a makeCahceMatrix object for input into the cacheSolve function.  

makeCacheMatrix <- function(x = matrix()) { # initializes x & sets the default value of x for the makeCacheMatrix to an empty matrix
  i <- NULL                 # initializes i and sets it to NULL   
  set <- function(y) {      # initializes y within the environment of set
    x <<- y                 # assigns the input of y to x in the environment of the parent function makeCacheMatrix
    i <<- NULL              # assigns value of NULL to i in makeCacheMatrix environment
  }                         # the defined set function allows the input of a new matrix without calling the makeCacheMatrix function more than once
  get <- function() x        # retrieves x from the makeCacheMatrix environment
  setsolve <- function(solve) i <<- solve # defines solve as the function that sets i 
  getsolve <- function() i  # retrieves i from the makeCacheMatrix environment
  list(set = set, get = get, 
       setsolve = setsolve, getsolve = getsolve) # creates a named list of the defined functions within makeCacheMatrix
  } 


## This function returns the inverse of a matrix, either through calculation or returning
# a previously cached value. 

cacheSolve <- function(makeCacheMatrix.object, ...) { 
        ## Return a matrix that is the inverse of 'x' that is input into the makeCacheMatrix
  i <- makeCacheMatrix.object$getsolve() # retrieves i from the makeCacheMatrix environment and assigns it to i 
  if(!is.null(i)) {   # if there was a cached value for i, this returns it with a message indicating it was cached
    message("getting cached data")
    return(i)
  }   # if i not returned (because it wasn't cached previously for the current matrix) the function continues to run
  data <- makeCacheMatrix.object$get() # assigns the matrix input in makeCacheMatrix to "data"
  i <- solve(data, ...) # solves the input matrix and assigns solution to i
  makeCacheMatrix.object$setsolve(i) # sets the solution i to the input object makeCacheMatrix.object, so if the same makeCacheMatrix.object is input, the cached solution will be retrieved 
  i # shows the solution in the console
}
