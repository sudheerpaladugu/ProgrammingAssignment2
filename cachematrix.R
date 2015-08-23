## makeCacheMatrix function takes martrix input 
## Operations
## set - stores input matrix in cache
## setmatrix - stores input value (m) in cache
## get - Returns matrix from Cache
## getmatrix - Returns stored value (m) from cache
makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    
    set<-function(y)
    {
        x<<-y
        m<<-NULL
    }
    
    get<-function() x
    
    setmatrix<-function(solve) m<<- solve
    
    getmatrix<-function() m
    
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
    
}


## This function computes inverse matrix and returns it
## it returns cached inverse matrix if it exist. Otherwise it will recompute
## inverse matrix of in put matrix, store it in cache, and returns it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getmatrix()
    
    if(!is.null(m))
    {
        message("Getting inversed matrix from cache...")
        return(m)
    }
    matrix<-x$get()
    
    m<-solve(matrix, ...)
    
    x$setmatrix(m)
    
    m
}
