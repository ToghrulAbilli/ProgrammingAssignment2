## makeCacheMatrix avoids calculating repeatedly the inverse of a matrix
# by storing the result in the parent environment and then retrieving
# if it's necessary

## Returns a list of functions that will handle operations
# over the matrix

makeCacheMatrix <- function(x = matrix()) {
          
     inv <- NULL
     
     set<-function(y){
       x <<- y
       inv <<- NULL 
     }
     
     get<-function() x
     
     setInverse   <- function(solve) inv <<- solve
     
     getInverse<-function () inv
     
     list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  

}

## calculates the inverse of a cached matrix created with 
# makeCacheMatrix. This function assumes that that the matrix is invertible.

cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv<- solve(data)
  x$setInverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}




