## Put comments here that give an overall description of what your
## functions do
## I have used the same ANALOGY for both functions as seen for the 
## vectors Function's in coursera website.

# makeCacheMatrix creates a list containing a function to

# Assuming that a matrix is given to the function as input
# 1. set the value of the matrix

# Pulling the matrix to a get 
# 2. get the value of the matrix

# Inverse of the matrix is made here
# 3. set the value of inverse of the matrix

# Te inverse matrix is loaded into get
# 4. get the value of inverse of the matrix



makeCacheMatrix <- function(x = matrix()) {
  
# I have asigned NULL value for "inv" to initialize it.
  inv <- NULL
  
  set <- function(y) {
          x <<- y
          inv <<- NULL
  }

# Here 'x' is a inline argument for the function,which is indeed
# the input for the function itself.
  get <- function() x

# Once inv gets calculated after the first iteration, we SET and GET.  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


# The following function returns the inverse of the matrix. If the first iteration
# is already completed then the cache will have the inverse matrix. so,It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {

# the getinverse is taken from the list(output of makeCacheMatrix) as assignd to inv.  
  inv <- x$getinverse()

  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }

# Since we dont have a cache already we calcukate Inverse here.  
  origialMatrix <- x$get()
  inv <- solve(origialMatrix)
  
# We have to assign it to setinverse with the computed inverse Matrix.  
  x$setinverse(inv)
  inv
}
