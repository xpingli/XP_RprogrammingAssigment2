## makeCachematrix takes a matrix and calculate its inverse matrix and store the result in the global environment. CacheSolve function can retrieve the inverse matrix assumed the inverse matrix is already in the global environment, if not it will calculate the inverse of given matrix.

## This function takes a matrix "x" as an input. "m" is a empty matrix. "set" function takes the input and stores it in the global environment in "y". "get" displays "x". "setinverse" function can solve "x" and pass the result in "m". if you want to retrieve "m", you can use "getinverse" function. makeCacheMatrix stores "set", "get", "setinverse","getinverse" in a list.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set<-function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cachSolve takes makeCacheMatrix(x) argument. It scans if the inverse is already in the environment and if it's there, it returns the inverse matrix and prompted the message "getting cached data". If it can't find the inverse matrix in the environment, it retrieves the input matrix from makeCacheMatrix(x) and calculate the inverse and output it. 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        ## Return a matrix that is the inverse of 'x'
        m
}
