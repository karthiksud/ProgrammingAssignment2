## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix is used to create the cachable matrix. 

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  ## "get" retrieves the value of x
  get<-function() x
  
  ## setmatrix runs the function with solve
  setmatrix<-function(solve) m<<- solve
  
  ## getmatrix retrives the value of m
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## The inverse of matrix is calculated by cacheSolve function.
## cachesolve calculates the inverse of matrix (of the list) created above (inverse calculated using Solve function)

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  
  ## If inverse has already been calculated, the function uses the inverse stored in cache
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  ## if inverse is not already calculated, it calculates the inverse and using set function, sets it to cache
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
