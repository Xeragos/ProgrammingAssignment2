## Functions designed to cache the inverse of a matrix

## Creation of special Matrix Object to cache its Inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Inverse Property initialization to be used later.
  i <- NULL
  
  ##Setting the matrix Function
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ##Getting the Matrix
  get <- function() {
    ## Print / Return the Matrix
    m
  }
  
  ## Setting the inverse of the matrix using this function
  setI <- function(inverse) {
    i <<- inverse
  }
  
  ## Getting the inverse of the matrix using this function
  getI <- function() {
    ## Return the inverse property
    i
  }
  
  ## Return the list of methods
  list(set = set, get = get,
       setI = setI,
       getI = getI)

}

## Computing inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse is already calculated with no change in matrix, 
## then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
        m <- x$getI()
        
        #Returning Inverse if the value is already set
        if( !is.null(m) ) {
          message("getting cached data")
          return(m)
        }
        
        ##Getting the matrix from our object
        data <- x$get()
        
        ##Calculating the inverse using matrix multiplication
        m <- solve(data) %*% data
        
        ## Set the inverse to the object
        x$setI(m)
        
        ## Return the matrix
        m
  
}
