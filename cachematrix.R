
## cacheMatrix.R
##
## The functions in this file provide a framework to cache the inverted form of
## a matrix. 
## 
## To use the functions: 
## - Pass in a matrix to the makeCacheMatrix() function, assigning the return value
##   to a variable.
## - Pass the return value of the makeCacheMatrix() function to the cacheSolve() function.
## - cacheSolve() will return the inverted matrix. 
## - Subsequent calls to cacheSolve() with the same makeCacheMatrix() functino return value 
##   will return a cached version of the Matrix, thereby avoiding the computationaly expensive 
##   solve call will return the previously calculated inverted matrix.
## 


## makeCacheMatrix
## Constructs an "object" that provides for storing a retieving a matrix. 

makeCacheMatrix <- function(x = matrix()) {
  
      m <- NULL
      set <- function (y){
        x <<-y
        m <<- NULL
        
      }
      get <-function() x
      setMatrix <- function(matrix) m<<-matrix
      getMatrix <-function() m
      list(set = set, get = get, 
           setMatrix = setMatrix,
           getMatrix = getMatrix)
}


## Takes an object created by the function makeCacheMatrix() representing a matrix
## and returns the inverse of that matrix. If the invers matrix has not been previously
## calculated, the function calculates the inverse and stores it in the object. If the 
## inverse matrix has been previously calculated, the function retrieves and returns 
## the cached inverse matrix, and a message indicating that cached data was used is printed.

## No exception handling is done to check that the matrix is invertable. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getMatrix()
        if(!is.null(m)){
          message ("getting cached data")
          return(m)
        }
        data <- x$get()
        m<-solve(data,...)
        x$setMatrix(m)
        m
}
