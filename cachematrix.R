##Written by Derek Boerner
##Coursera - R Programming Assignment 2

##create main function containing list of functions

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  #function to set the matrix when passed via argument
  set <- function(y) {
    x <<- y #set variable x to new matrix
    i <<- NULL #clear variable i since inverse matrix not yet calculated
  }
  #function to return the current matrix
  get <- function() x
  #function to set the inverse matrix when passed via argument
  setinv <- function(solve) i <<- solve
  #function to return the inverse matrix
  getinv <- function() i
  #create list of subfunctions that can be called using main function with $
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##create function where input is the object where makeCacheMatrix is stored

cacheSolve <- function(x, ...) {
  #set variable i to result of subfunction $getinv
  i <- x$getinv()
  #check to see if variable i is null and return it if not null
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  #else get matrix from subfunction $get
  data <- x$get()
  #calculate the inverse matrix and set to variable i
  i <- solve(data, ...)
  #call subfunction $setinv to store inverse matrix
  x$setinv(i)
  #return inverse matrix
  i
}
