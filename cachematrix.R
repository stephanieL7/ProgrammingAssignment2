## Functions to solve a matrix and cache it's inverse. 

## This function caches the inverse of a matrix. 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL                     
  set <- function(y) {       
    x <<- y
    i <<- NULL
  }
  get <- function() x        
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i 
  list(set = set, get = get, 
       setsolve = setsolve, getsolve = getsolve)
  }


## This function returns the inverse of a matrix, either through calculation or returning
# a previously cached value. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  i
}





makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(makeVector.object, ...) {
  m.local <- makeVector.object$getmean()
  if(!is.null(m.local)) {
    message("getting cached data")
    return(m.local)
  }
  data <- makeVector.object$get()
  m.local.calculated <- mean(data, ...)
  makeVector.object$setmean(m.local.calculated)
  m.local.calculated # return the mean value
}


vector1 <- makeVector(1:20)
cachemean(vector1)

vector2 <- 1:15

makeVector.object$set(vector2)

cachemean(makeVector.object)
