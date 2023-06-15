## Functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( A = matrix() ) {
  
  ## Initialize the inverse property
  I <- NULL
  
  ## To set the matrix
  set <- function(y) {
    A <<- y
    I <<- NULL
  }
  
  ## To get the matrix
  get <- function() {
    ## Return the matrix
    A
  }
  
  ## To set the inverse of the matrix
  setinv <- function(invcal) {
    I <<- invcal
  }
  
  ## To get the inverse of the matrix
  getinv <- function() {
    ## Return the inverse property
    I
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



cacheSolve <- function(A, ...) {
  
  I <- A$getinv()
  
  
  if( !is.null(I) ) {
    message("getting cached data")
    return(I)
  }
  
  ## Get the matrix from our object
  data <- A$get()
  
  ## Calculate the inverse
  I <- solve(data,...)
  
  ## Set the inverse to the object
  A$setinv(I)
  
  ## Return the matrix
  I
}

###Test
A<-makeCacheMatrix(matrix(1:4,nrow = 2,ncol = 2))
A
A$get()
cacheSolve(A)
