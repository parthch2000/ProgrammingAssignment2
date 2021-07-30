## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## setinverse assigns the value of inverse matrix
## getinverse returns the value of inverse matrix

## The 2nd function, "cacheSolve" calculates the inverse of the special "matrix" created with the above function.
## It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

## Write a short comment describing this function

  makeCacheMatrix <- function(x = matrix()) { ## define the argument with default mode of "matrix"
    inv <- NULL                             ## initialize inv as NULL, it later holds value of matrix inverse 
    set <- function(y) {                    ## set function assigns new 
      x <<- y                             ## value of matrix in parent environment
      inv <<- NULL                        ## reset inv to NULL if there is a new matrix
    }
    get <- function() x                     ## returns value of the matrix
    setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
    getinverse <- function() inv                     ## returns the value of inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## for referring the functions with the $ operator
  }
  
  
  ## Write a short comment describing this function
  ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
  
  ## If inverse already calculated and no change in the matrix,
  ## then cacheSolve will retrieve the inverse from the cache
  
  cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
  }