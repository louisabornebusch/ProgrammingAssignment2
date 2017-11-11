## A pair of functions that cache the inverse of a matrix
## makeCacheMatrix:creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  ## this initializes the inverse property
  m<-NULL
  
  ## a method to set the matrix
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  ## This is the method the get the matrix
  get <- function() {
    ## Return the matrix
    x
  }
  ## Now we want to set the inverse of the matrix using
  setInverse <- function(inverse) {
    i <<- inverse
  }
  ## To get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    m
  }
  ## To return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        ## Return the inverse if its already set
        if( !is.null(m) ) {
          message("getting cached data")
          return(m)
        }
        ## Get the matrix from our object
        data <- x$get()
        ## Calculate the inverse using matrix multiplication
        m <- solve(data) %*% data
        ## Set the inverse to the object
        x$setInverse(m)
        ## Return the matrix
        m      
}

