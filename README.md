##The function makeCachematrix creates a special "matrix" object which can cache its inverse 
 makeCachematrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) m <<- inverse
            getinverse <- function() m
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
    }
    ##The cacheSolve function computes the inverse of the special "matrix" returned my makeCachematrix and compares it with the contents of the cache
 cacheSolve <- function(x, ...) {
            m <- x$getinverse()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setinverse(m)
            m
    }

