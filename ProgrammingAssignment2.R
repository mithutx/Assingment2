## ------Programming Assignment 2 -------
## ------Name: Mostafizur Rahman  -------

## ------makeCacheMatrix function-------
## This function returnes a list of 4 functions
makeCacheMatrix <- function(x=matrix()){
  invX <- NULL
  set <- function(y) {
    x <<- y
    invX <<- NULL
  }
  get <- function() x
  setInv <- function(solve) invX <<- solve
  getInv <- function() invX
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## ------cacheSolve function-------
## This function checks if the matrix is invertable. If it is,
## it inverts the matrix.
## When it's called the second time, it just returns the inverted function without
## going through the inversion operation
cacheSolve <- function(x, ...) {
  invX <- x$getInv()
  if(!is.null(invX)) {
    message("getting cached data")
    return(invX)
  }
  data <- x$get()
  detInv <- det(data, ...)
  if(detInv==0) {
    message("matrix is not invertable")
    invX <<- NULL
    return(invX)
  }
  invX <- solve(data, ...)
  x$setInv(invX)
  invX
}
