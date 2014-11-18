## This script comprises two functions. It makes inversion of a given matrix and store it in the cache
##for further process
##Author: Yuan Cheng, November 17, 2014

## makerCacheMatrix takes a matrix and construct a list of getter and setter and return a list
##to be used by cacheSolve method

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
    
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve performs inversion of a given matrix 'x' and store it in the cache by using
## setter and getter functions provided in makerCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
          
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        
        inv
}
