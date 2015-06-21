## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly. The
## following two functions are used to cache the inverse of a matrix.


## 1) makeCacheMatrix
##Returns a list of functions containing:
##set the value of the matrix
##get the value of the matrix
##set the value of inverse of the matrix
##get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {    ##Function that Changes the vector stored in the main function.
    x <<- y
    inv <<- NULL
  }
  get = function() x     ##Function that returns the vector x stored in the main function.Doesn't require any input.
  setinverse = function(inverse) inv <<- inverse ## Store the value.
  getinverse = function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## 2)cacheSolve
##Used to calculate the inverse of a matrix. First it checks whether the inverse is already calculated.
##If so,it doesn't calculate it again.Else,it calculates the inverse and sets the value in the cache via setinverse function
##Function assumes that the matrix is invertible.
cacheSolve <- function(x, ...) {
       
  inv = x$getinverse()
  if (!is.null(inv)){ ##Checks whether inverse already exists.
     
    message("getting cached data")
    return(inv)
  }
  
  data = x$get()
  inv = solve(data, ...)
  x$setinverse(inv)
  
  return(inv)
}
