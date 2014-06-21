## The two functions cache the matrix in the environment of the function makeCacheMatrix and make it (and its inverse)
## available for later use without the necessity to compute the inverse again.

## creates a list of 4 functions (set, get, setinverse, getinverse)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                   # inv is "inverse" and the information if invers has already been calculated
  set <- function(y) {        # opens a new environment of the uncalled function "function(y)"
    x <<- y                   # applys the deliverd matrix into the makeCacheMatrix - environment
    inv <<- NULL              # sets invers in the makeCacheMatrix - environment to zero
  }
  get <- function() x                         # passes the deffined matrix
  setinvers <- function(invers) inv <<- invers  # calculates the inverse 
  getinvers <- function() inv                     # can "inverse" be gotten, or is it to be calculated?
  list(set = set, get = get,                  # returns the list of functions
       setinvers = setinvers,
       getinvers = getinvers)
}


## once the matrix was defined using the makeCacheMatrix-function by 
# > makeChacheMatrix(x)
# cacheSolve can check if the inverse of the function was already calculated and
    # if it was, pull it from the cache of the makeCacheMatrix-environment
    ## or calculate it and cache it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinvers()                  
  if(!is.null(inv)) {                   # "asks" if invers can be gotten or has to be calculated
    message("getting cached data")
    return(inv)                         # when inv != 0 then the "loop" is ended after returning the inv
  }                                     # when inv==0 then the lines 30-33 are executed
  data <- x$get()                       # the data is pulled from the makeCacheMatrix- Environment
  inv <- solve(data)                    # the inverse of the matrix is calculated
  x$setinvers(inv)                      # the inverse is set in the makeCacheMatrix - Environment
  inv                                   # the inverse is returned
}

## test sample:
# t <- matrix(c(1,2,3, 11,12,13, 25, 36, 98), nrow = 3, ncol = 3)
# M <- makeCacheMatrix()
# M$set(t)
# cacheSolve(M)
# cacheSolve(M)
