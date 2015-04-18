## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function() x
    set <- function(y){
        x   <<- y
        inv <- NULL
    }
    getInv <- function() inv
    setInv <- function(inverse) inv <<- inverse
    list(set = set, get = get, setInv = setInv,
         getInv = getInv)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)){
        message("Get the cached data!")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInv(inv)
    inv
}
