## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


makeCacheMatrix <- function(x = matrix()) {
    ## Just checking out if we have square matrix
    if (nrow(x)!=ncol(x)) message(paste("Sorry bro, nothing happens. You need to have square matrix, but you have", nrow(x),"by", ncol(x), "one."))
    else {
    inv <- NULL ##Cleaning the content of inverted matrix
    set <-function(y) {
        x<<-y ## Assigning 'y' to 'x' under makeCacheMatrix env. If x is not defined already, it will set as 'y' in global environment
        inv<<-NULL ##'Voiding' inverted matricx 
    }
    get <- function() x ##returns the initial matrix
    setinv <-function(solve) inv <<- solve ##calulating inverted matrix in terms of function
    getinv <- function() inv ##returning the inverted matrix
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
    }
}


## Write a short comment describing this function

## Now will get our inverted matrix...

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv() #setting inverted matrix as the result of prevous func calculation
    if(!is.null(inv)) {
        message("Here we go!")
        return(inv)
    } ##If it has been calculated earlier, our CPU's having fun
    message("Holy cow, it wasn't cached yet, but we'll fix that!") ##otherwise it should be calculated
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
