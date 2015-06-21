## makeCacheMatrix takes a matrix as input and return a list 
## It also has 4 functions using which the matrix value can be obtained(get())
##changed(set()), inverse can be set(setinv()) and inverse can be obtained(getinv())

## this function receives x as an input matrix and return a list containing value
## of inverse of square matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  get<-function()x
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  getinv<-function()inv
  setinv<-function(inverse)inv<<-inverse
  list(get=get,set=set,getinv=getinv,setinv=setinv)
}


## this function either returns value of inverse of matrix if already assigned
##or else computes it using solve() function

cacheSolve <- function(x,...) {
  inv<- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv          ## Return a matrix that is the inverse of 'x'
}
