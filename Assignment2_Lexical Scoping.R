#1) makeCacheMatrix
        #This function creates a special "matrix" object that can cache its inverse.
       
makeCacheMatrix<-function(x=matrix()){
        invmtx<-NULL
        set<-function(y) {
                x<<-y
                invmtx<<-NULL
        }
        get<-function()x
        setinv<-function(inverse) invmtx<<- inverse
        getinv<-function() invmtx
        list(set=set,
             get=get,
             setinv=setinv,
             getinv=getinv)
}

#2) cacheSolve 
        #This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
        #If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will retrieve the inverse from the cache.

cacheSolve<-function(x,...) {
        invmtx<-x$getinv()
        if(!is.null(invmtx)){
                message("getting cached data")
                return(invmtx)
        }
        data<-x$get()
        invmtx<-solve(data, ...)
        x$setinv(invmtx)
        invmtx
}