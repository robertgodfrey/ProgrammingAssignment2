##  The first function creates a special matrix object that can cache 
##  the inverse of the object
#
## The second function performs the inversion of the matric object created
## in the first function

## This creates the matrix object and has the ability to call the second object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    # use `<<-` supr assignment operator to set a variable within an environment 
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse 
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function uses the Solve function to invert the matrix passed from
## the first function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        # if the inverse has already been calculated
        if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
          
          # else it calculates the inverse 
          data <- x$get()
          inv <- solve(data, ...)
          
          # sets the value of the inverse in the cache.
          x$setinv(inv)
          return(inv)
}
