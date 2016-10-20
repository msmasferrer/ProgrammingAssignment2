## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m=matrix()) {
  x <- NULL
  set <- function(y) {
    m <<- y
    x <<- NULL
  }
  
  get <- function() m
  setInv <- function(solveMatrix) x <<- solveMatrix
  getInv <- function() x
  list(set=set, get=get, setInv=setInv, getInv = getInv)
}

## Write a short comment describing this function

# Now we create the function cacheSolve to compute the inverse of the matrix 
# produced by the function above

cacheSolve <- function(m, ...) {
  x <- m$getInv()
  if(!is.null(x)) {
    paste("Cached", "solution", sep =  " ")
    return(x)
  }
  data <- m$get()
  x <- solve(data)
  m$setInv(x)
  x
}