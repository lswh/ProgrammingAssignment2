## The first function is an object that stores a matrix. The second function caches the mean of the matrix
## stored from the first function defined. This is a programming assignment for R.
## If vectors use the mean function, this one made use of the solve function


## This creates a cache for a given matrix and lists its relevant parameters for its inverse
makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y) {
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(inverse) m<<- inverse
    getmatrix<-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}


## This retrieves a cached value of the inverse matrix of x. 

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
