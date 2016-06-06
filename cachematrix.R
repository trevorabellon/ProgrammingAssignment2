## These functions will create a vector that will store the inverse
## of an input matrix and cache the value of the inverse matrix

## This function creates a list of functions to set and store 
## the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv_x<-NULL
        set<-function(y){
                x<<-y
                inv_x<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) inv_x<<-inverse
        getinverse<-function() inv_x
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## This function checks to see if the inverse matrix has been cached.
## If it has, it will retreive it from the previous function.
## If not, it will calculate the inverse using the solve() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x<-x$getinverse()
        if(!is.null(inv_x)){
                message("getting cached data")
                return(inv_x)
        }
        data<-x$get()
        inv_x<-solve(data,...)
        x$setinverse(inv_x)
        inv_x
}
