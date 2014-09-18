## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#created by harakiri85

#this function will create an object that holds
# a matrix and the inverse of it

makeCacheMatrix <- function(x = matrix()) {
    
    inversa <- NULL
    set <- function(y) {
        #save data and inversa to null
        x <<- y
        inversa <<- NULL
    }
    get <- function() x
    
    #save inversematrix
    setinverse <- function(inverse_m) inversa <<- inverse_m
    
    #return saved inversematrix = inversa
    getinverse <- function() inversa
    list(set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse )
}


## Write a short comment describing this function
#this function will check if the inverse is already saved. 
#If no, it will calculate it and save it for future use.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inversa <- x$getinverse()
    if(!is.null(inversa)) {
        message("getting cached inverse matrix")
        return(inversa)
    }
    #if inversa was null, y have to calculate it
    #first I get the matrix data
    data <- x$get()
    #calculate the inverse
    inversa <- solve(data, ...)
    x$setinverse(inversa)
    inversa
    
}

