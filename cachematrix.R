## makeCacheMatrix function takes martrix input 
## Operations
## set - stores input matrix in cache
## setmatrix - stores input value (m) in cache
## get - Returns matrix from Cache
## getmatrix - Returns stored value (m) from cache
makeCacheMatrix <- function(x = matrix()) {
    #assigning default value
    m<-NULL
    #setting up input matrix value x and NULL to m (in cache) 
    set<-function(y)
    {
        #assgning input matrix to x
        x<<-y
        #resetting m value
        m<<-NULL
    }
    #returns x value from cache
    get<-function() x
    #setting inverse matrix 'm' to chace (expected input inverse matrix)
    setmatrix<-function(invm) m<<- invm
    #returns inverse matrix 'm from cache
    getmatrix<-function() m
    #setting up operatoins to a list
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
    
}


## This function computes inverse matrix and returns it
## it returns cached inverse matrix if it exist. Otherwise it will recompute
## inverse matrix, stores in cache and returns it.
cacheSolve <- function(x, ...) {
    #getting invermatrix of matrix 'x', from cache    
    m<-x$getmatrix()
    #checking for inverse matrix value exist in cache
    if(!is.null(m))
    {
        message("Getting inversed matrix from cache...")
        #returns inverse matrix of 'x' from cache
        return(m)
    }
    #Getting cached matrix value from cache
    matrix<-x$get()
    #computing inverse matrix of input matrix
    m<-solve(matrix, ...)
    #setting re-computed inverse matrix in cache
    x$setmatrix(m)
    #return inverse matrix
    m
}
