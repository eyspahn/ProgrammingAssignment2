##eyspahn on 20Mar2015 for Coursera R course 
##example matrix  mat<-matrix(c(1,2,1,1,2,3,3,2,2),nrow=3,ncol=3,byrow=TRUE)


#This function creates a special "matrix" object 
#that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m<- NULL
    set<- function(y){
        x<<-y
        m<<-NULL
    }
    get<- function() x
    setinv<-function(matinv) m<<-matinv
    getinv<- function() m

    #return a list of functions
    list(set = set, get = get, setinv=setinv, getinv=getinv)
    
}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

#Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <-x$getinv()
    if(!is.null(m)) {
        message("getting inverted matrix")
        return(m)
    }
    mat<-x$get()
    m<-solve(mat)
    x$setinv(m)
    m

}
