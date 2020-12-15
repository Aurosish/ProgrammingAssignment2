## Created a function that creates a special "matrix" object and 
##caches its inverse.
## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) { 
  mat_inv <- NULL                     
  set <- function(y) {
    
    x <<- y
    mat_inv <<- NULL 
    ##Changes the vale of matrix if the matrix changes
  }
  
  get <- function() x                           
  setinverse <- function(solve) mat_inv <<- solve 
  ##set inverse of matrix using "solve" function.
  getinverse <- function() mat_inv        
  ##gets the inverse.
  list(set = set, get = get,                    
       setinverse = setinverse,
       getinverse = getinverse)
}


## Created a function that looks for inverse in the cache and if not 
## found, calculates the inverse using the "solve" function.
cacheSolve<- function(x, ...) {                 
  ##function takes argument as "makeCacheMatrix(p)" where p is 
  ##the matrix whose inverse is to be found.
  mat_inv <- x$getinverse()

  if(!is.null(mat_inv)) {                 
    message("getting Inverse of matrix from cached data")
    return(mat_inv)
    ## If inverse already exists it is returned.
  }
 
  data <- x$get()                               
  mat_inv <- solve(data, ...)
  x$setinverse(mat_inv)
  mat_inv
  ## Return a matrix that is the inverse of 'x'
}