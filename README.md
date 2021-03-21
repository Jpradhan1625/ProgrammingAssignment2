# ProgrammingAssignment2
## THe two functions together makeCacheMatrix and cacheSolve together creates a special object 
## that stores the inverse of a matrix. It will not recalculate the inverse if it already exists.
## When you create a makeCacheMatrix object by calling the function with a matrix as an argument
## it assigns that matrix to variable x. Then it returns a list of functions that are available 
## to get or set the x variable (i.e. the matrix) or to get or set the m variable which is the inverse
## matrix of x. Though the set and setInverse functions set the values of x and m inside the functions
## throught the user of the <<- operator the scope of those variables is outside the function.

## makeCacheMatrix This creates the object that saves the matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## taking an object of the type cacheMatrix cacheSolve function provides the methods
## to calulate the inverse if it does not already exist. This function can use a makeCacheMatrix
## to calculate the Matrix Inverse only if it does not exist. If it already exists then
## the calculated value is returned.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
