makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<-y
    m<<-NULL
    }
  get <- function() x
  setinverse <- function (Inverse) m <<-Inverse
  getinverse <-function() m
  list(set=set, get=get,getinverse=getinverse,setinverse=setinverse)
}

+## Cache inversematrix
  
  cacheSolve <- function(x, ...) {
    +        m <- x$getinverse()
    +        if(!is.null(m)){
      +                message("getting cached data")
      +                return(m)
      +                }
    +        data <- x$get()
    +        m <- Solve(data,...)
    +        x$setinverse(m)
    +        m
    ## Return a matrix that is the inverse of 'x'
  }
