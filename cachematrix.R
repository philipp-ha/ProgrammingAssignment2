## makeCacheMatrix creates a matrix which can store its inverse in the global environment
## cacheSolve returns the inverse of makeCacheMatrix and calcuates it if it doesn't exist already


## makeCacheMatrix takes a matrix as an argument and stores its value in x and can store the inverse in i

makeCacheMatrix <- function(x = matrix()) {
  i=NULL
  set<- function(y){
    x<<-y 
    i<<-NULL
  }
  get<- function() x
  setinverse<- function(inverse) i<<-inverse
  getinverse<-function() i
  list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve takes the matrix from makeCacheMatrix and returns the inverse. 
## it Checks if its inverse is stored already. If not it calculates and stores the inverse.

cacheSolve <- function(x, ...) {
  i<-x$getinverse()     ## Return a matrix that is the inverse of 'x'
  if(!is.null(i)){
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$setinverse(i)
  i 
}
