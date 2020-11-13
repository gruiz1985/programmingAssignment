## The function calculate de inverse of a given matrix. In case the inverse is already
## calculated the function get the solution from cache

## Crearte a list with the information of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x
  setinverse<-function(solve) i<<-solve
  getinverse<-function() i
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## Calculate de inverse of a matrix, in case it is alredy calculated the function
## take it from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
