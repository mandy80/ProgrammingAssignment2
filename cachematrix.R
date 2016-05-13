## This program has two functions named: makeCacheMatrix, and 
## cacheSolve. makeCachedMatrix returns a list of four function which can
## initialize a matrix, return the matrix, set inverse of the matrix, and 
## return the inverse matrix if already present. cacheSolve function 
## checks if the inverse of a matrix is already set or not, if set, it
## prints the matrix otherwise calls the seti function of makeCachedMatrix 
## to find the inverse.

## This function has a list of four functions in it. It takes a matrix as
## argument. Although no check is being applied to the input Matrix but 
## it is assumed it will be square matrix. These functions set the matrix, 
## get the matrix and print it, set the inverse matrix provided to them
## and last function returns the inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinv <- function(solve) i <<- solve
      getinv <- function() i
      list(set = set, get = get,
           seti = setinv,
           geti = getinv)
}


## This function actually evaluates the inverse for the provided matrix.
## Once evaluated, it stores it in variable i and for later usage it only
## check if i is null. If i is null means the inverse of the matrix has 
## not been evaluated yet. SO it evaluates it for the first time. For any
## later use, it only needs to access this variable which resides in main
## memory.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      i <- x$geti()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$seti(i)
      i
}
