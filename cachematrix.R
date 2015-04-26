cachematrix <-function(){
## Stores and returns inverse of matrix, if matrix is a new invertible matrix it calculates the new inverse and stores its

## makeCacheMatrix stores functions; get which retrieves the matrix stored in the main function, set which changes the matrix stored in the main function if it doesnt match, setsolve stores the value of the input, while getsolve returns the value to the main function makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve verifies the value of the inverse matrix m stored in getsolve, if not stored it calculates
## the inverse matrix input of the makeCacheMatrix function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m) #stores the inverse in makeCacheMatrix
  m
}
}