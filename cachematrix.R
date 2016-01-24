
#makeCacheMatrix creates a special "matrix" object that can cache/store its inverse, which is calculated using cacheSolve. It "sets the stage" for the cacheSolve function to have an object to look into
# if the inverse has already been calculated, cacheSolve will find it in the "matrix" and return it instead of calculating again

makeCacheMatrix<- function(x=matrix()){
        invx<- NULL
        setmatrix<- function(y){ 
                x<<- y
                invx<<- NULL 
        }
        get<- function() x
        setinverse<- function(inverse) invx <<- inverse
        getinverse<- function() invx
        list(setmatrix=setmatrix, get=get, setinverse=setinverse, getinverse=getinverse)
}


#cacheSolve computes and returns the inverse of the special "matrix" 
# created by makeCacheMatrix
#if the inverse has already been calculated (and the matrix has not
#changed), then cacheSolve should retrieve the inverse from the cache
#if it has not been previously calculated, cacheSolve computes, caches
#and returns it
cacheSolve<- function(x, ...){
        invx<- x$getinverse()
        if(!is.null(invx)){ 
                message("getting cached data")
                return (invx)
        }else {
                invx<- solve(x$get())
                x$setinverse(invx) 
                return(invx) 
        }
}