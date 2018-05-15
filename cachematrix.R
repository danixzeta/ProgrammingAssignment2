#the first function makeCacheMatrix creates a special matrix like variable
# that store the matrix and its inverse once calculated.
#it is a list containing functions to 
#1.set the value of the matrix
#2 get the value of the matrix
#3 set the value of the inverse
#4 get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        m<-x$getinverse()## Return a matrix that is the inverse of 'x'
    if(!is.null(m)){message('getting Cached inverse')
        return(m)}
    mat<-x$get()
    if(ncol(mat)!=nrow(mat)){stop('Error - matrix is not square')}
    if(det(mat)==0){stop('ERROR: matrix is not invertible')}
    m<-solve(mat, ...)
    x$setinverse(m)
