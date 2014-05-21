## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function, `makeCacheMatrix` creates a list containing functions to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the matrix's inverse'
## 4.  get the value of the matrix's inverse'

makeCacheMatrix <- function(x = matrix()) {
  
  I <- NULL
    set <- function(y) { # the 'set' function
      x <<- y
      I <<- NULL
    }
    get <- function() x # the 'get' function
    setinv <- function(inv) I <<- inv # the 'setinv' function
    getinv <- function() I # the 'getinv' function
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}

## Write a short comment describing this function

# The following function calculates the inverse of a given matrix.
# It first checks to see if the inverse is already calculated and 'cached'.
# If so, it simply gets the matrix inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the inputted matrix
# and sets the value of the inverse in the cache via the setinv function.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  I <- x$getinv()
  if(!is.null(I)) { # to check if the inverse already exists or not.
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...) # if cache is empty, recalculate inverse
  x$setinv(I)
  I
  
}
