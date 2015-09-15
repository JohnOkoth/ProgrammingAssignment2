
####
#My functions gets the matrix for the data entered from makeCacheMatrix function, 
# then an inverse matrix for the entry is solved by cacheSolve, with the inverse being
# returned from the cache after calculation; so long as the orginal matrix is still the
# same.



#This function gets the matrix for the data entered from makeCacheMatrix function, 
#to create a special matrix that will be able to cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  j<-NULL
  set<-function(y){
    x<<-y
    j<<-NULL
  }
  get<-function() x
  setinverse<-function(solve) j<<-solve
  getinverse<-function() j
  list(get=get, set=set, setinverse=setinverse, getinverse=getinverse)
}


 #This function calculates the inverse of the special matrix returned by makeCacheMatrix function.


cacheSolve<-function(x, ...){
  
  j<-x$getinverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  data<-x$get()
  j<-solve(data, ...)
  x$setinverse(j)
  j
}




