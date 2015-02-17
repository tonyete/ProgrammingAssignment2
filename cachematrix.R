## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:
## This function creates a matrix that can cache its inverse.
## The function makeCacheMatrix returns a list o functions with
## the following utilities:
## 1. set - save the matrix
## 2. get - return the matrix
## 3. setInverse - save the inverse
## 4. getInverse - return the inverse


makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix<-NULL
    set<-function(y){
        x<<-y
        inverseMatrix<<-NULL
    }
    get<-function() x
    setInverse<-function(inverse) inverseMatrix<<-inverse
    getInverse<-function() inverseMatrix
    list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function takes a matrix "x" and checks if the inverse has already been calculated
## If the inverse has been calculated and the matrix hasn't changed, then 
## returns the inverse stored in cache. 
## If the matrix has changed the function saves it and solves the inverse.
## If the inverse it hasn't been calculated yet, it solves and returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverseMatrix<-x$getInverse()
    if(!is.null(inverseMatrix) && x==x$get){
        message("getting cached inverse of the given matrix")
        return(inverseMatrix)
    }
    if(x!=x$get){x$set(x)}
    matrix<-x$get()
    inverseMatrix<-solve(x)
    x$setInverse(inverseMatrix)
    inverseMatrix
}
