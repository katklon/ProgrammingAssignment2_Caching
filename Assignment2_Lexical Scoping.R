#1) makeCacheMatrix
        #This function creates a special "matrix" object (cmtx) that can cache its inverse (invmtx).
       
makeCacheMatrix<-function(cmtx=matrix()){
        invmtx<-NULL
        set<-function(y) {                           #sets value of "matrix" object (cmtx)
                cmtx<<-y
                invmtx<<-NULL
        }
        get<-function()cmtx                          #delivers value of "matrix object"(cmtx)
        setinv<-function(solve) invmtx<<- solve      #sets value of inverse matrix (invmtx)
        getinv<-function() invmtx                    #delivers value of inverse matrix (cmtx)
        list(set=set,
             get=get,
             setinv=setinv,
             getinv=getinv)
}

#2) cacheSolve 
        #This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
        #If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will retrieve the inverse from the cache.

cacheSolve<-function(cmtx,...) {
        invmtx<-cmtx$getinv()
        if(!is.null(invmtx)){
                message("getting cached data")
                return(invmtx)
        }
        data<-cmtx$get()
        invmtx<-solve(data, ...)
        cmtx$setinv(invmtx)
        invmtx
}