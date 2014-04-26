## Below are two functions that are used to create a special object that stores 
## a square matrix and cache its inverse.


###################################################


# The following function receives a matrix "x". 
# However, it first checks to see if the matrix is square. 
# If so, it creates a special "matrix", which is  a list containing a function to:
# set the value of the matrix
# get the value of the matrix
# set the value of the matrix inverse
# get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        
        # Check if the matrix is square. If not throw error message
        if(!ncol(x)==nrow(x)){
                stop(message("Error: The object is not a square matrix"))}
        
        cacheMatrix <- NULL
        
        set <- function(y){
                x <<- y
                cacheMatrix <<- NULL
        }# End set()        
        
        get <- function() x
        setinverse <- function(inverse) cacheMatrix <<- inverse        
        getinverse <- function() cacheMatrix
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
        
} ## End of makeCacheMatrix()



####################################################


# The following function calculates the inverse of the special "matrix"
# created with makeCacheMatrix(). However, it first checks to see if the 
# inverse has already been calculated and cached. If so, it gets the inverse
# from the cache and skips the computation. Otherwise, it calculates the 
# inverse of the object "data" and sets the value of the inverse in the cache
# via the setinverse() function.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        
        cacheMatrix <- x$getinverse()
        
        if(!is.null(cacheMatrix)) {
                message("getting cached data")
                return(cacheMatrix)
        }
        
        data <- x$get()
        cacheMatrix <- solve(data, ...)
        x$setinverse(cacheMatrix)
        cachematrix
                
        
}# End of cacheSolve()




