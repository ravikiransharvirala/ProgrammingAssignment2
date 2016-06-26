## Matrix Inverse using makeCacheMatrix and cacheSolve
## functions do

## 1. set the value of matrix
## 2. get the value of matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve function returns the inverse of the matrix
## 1. It first checks if the inverse of matrix is calculated 
## 2. If it is done then it gets the result and skips the computation
## 3. if not the it calculates the inverse and returns the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
