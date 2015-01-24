## Put comments here that give an overall description of what your
## functions do

## This functon creates a special 'matrix' object that 
## can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    print(environment())
    evn <- environment()
    print(parent.env(evn))
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    #setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    getevn<- function() environment()
    list(set = set, get = get,
         setinversematrix = setmatrix,
         getmatrix = getmatrix,
         getevn = getevn)
  }

##This function computes the inverse of the special
## 'matrix' returned by makeCacheMatrix above
## if the inverse has already been calculated and the
## matrix has not changed, then cacheSolve should
## return the inverse from the cache

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  
  #attempt to get the inverted matrix
  m <- x$getmatrix() 
  
  #if the inverted matrix is available, return
  if(!is.null(m)){
    print("found cached matrix")
    return (m)
  }
  #else get the matrix
  newmatrix <-x$get()
  #invert the matrix
  m<-getInverseMatrix(newmatrix)
  #cache the new matrix
  x$setmatrix(m)
  m
  
}

##this function returns the inverse of a matrix
getInverseMatrix <- function(y){
  print("solving")
  solve(y) %*% y
} 


