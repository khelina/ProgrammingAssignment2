## 'makeCahchematrix' creates a special "matrix" object
## while 'cacheSolve' function computes the inverse of the special "matrix"returned

## 'makeCachematrix' creates a special "matrix" object that can cache its incerse

makeCacheMatrix <- function(x = matrix()) {
m<-NULL
set<-function(y){
x<<-y
m<<-NULL
}
get<-function() x
setInverse<-function(inverse) m<<-inverse
getInverse<-function() m
list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## 'cacheSolve' function computes the inverse of the special "matrix" returned by makeCachematrix
## and if the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
m<-x$getInverse()
if(!is.null(m)){
message("getting cached data")
return(m)
}
mat<-x$get()
m<-solve(mat,...)
x$setInverse(m)
m
}

