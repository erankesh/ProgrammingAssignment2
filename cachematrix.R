## Put comments here that give an overall description of what your
## functions do

## First the functions gets a matrix as an argument.
## Its inverse is stored in a object defined as i, which is set to NULL initially
## set funtion sets the new matrix in the object
## get function gets the value of matrix as it is
## setinverse sets the value of inverse to the i object defined in the parent function
## getinverse function gets the value of inverse stored in object i
## every function is returned as a list at the end of the funtion
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function gets x as an argument and all the other parameters as it is
## inverse of a matrix is collected from the function if there is any cache value in it
## if there exists any value it takes that value from the cache as return it
## if there is NULL value set by the functions then an inverse of the matrix is calculated
## In the example provided for the assignment 2 else statement was not there which
## calculates the value of inverse again that is why the else statement is provided 
## so that it will not get repeated
## the value of i if calculated is then cached in the variable by x$setinverse(i)
## value of inverse is returned by the object i
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  } else{
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    return(i)
  }
  return(i)
}
