## This function creates a matrix, caches its inverse and returns the inverse of the matrix. 
## It will be assumed that all matrix has an inverse and can be cached.

## Sets and gets the value of the matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y){
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function(){
        x
    }
    setinverse <- function(matrixInverse){
        inverseMatrix <<- matrixInverse
    }
    getinverse <- function(){
        inverseMatrix
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## If the previous function has calculated the inverse of the matrix, it returns it. If not, calculates it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    matInverse <- x$getinverse()
    if (!is.null(matInverse)){
        message("Matrix inverse found, getting cached data. . .")
    }else{
        message("Matrix inverse not found, calculating. . .")
        matrixData <- x$get()
        matInverse <- solve(matrixData, ...)
        x$setinverse(matInverse)
    }
    matInverse
}
