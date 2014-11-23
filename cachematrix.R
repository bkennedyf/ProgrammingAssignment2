## makeCacheMatrix creates a list of functions that operate on "internal" variables
#  allowing a matrix and its inverse to be cached
## cacheSolve returns the inverse of a matrix that has been defined by makeCacheMatrix,
#  it retrieves the cached value if it exists, otherwise it calculates the inverse and 
#  caches the value.

## makeCacheMatrix creates a list of functions that work with a matrix and
## can save the inverse so that it only needs to be calculated once

makeCacheMatrix <- function(x = matrix()) {
  
  inverseX = NULL;
  
  # set
  set <- function(newMatrix){
    print("in set")
    x <<- newMatrix;
    inverseX <<- NULL;
  }
  
  # get
  get <- function() x
  
  # set inverse
  setInverse <- function(newInverse) inverseX <<- newInverse
  
  # get inverse
  getInverse <- function() inverseX
  
  
  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)

}


## cacheSolve gets the inverse of the passed matrix from cache or, if it doesn't exist,
## solves for it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' unless it has already 
        # been calcuated
  
        inverseX = x$getInverse();
  
        if(is.null(inverseX)){
          print("Calculating Inverse")
          inverseX <- solve(x$get());
          x$setInverse(inverseX);
        }
        
        # return inverseX
        inverseX
}
