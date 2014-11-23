## The two functions below will calculate the inverse of a matrix x. 
## If the inverse has been cached as a result of a previous calculation for the same matrix, 
## the result is simply returned and no further calculation takes place.
##
## The "makeCacheMatrix" function creates an object that:
##  - Initializes a variable 'I'which will save the inverted matrix 
##  - Contains a function get()to obtain the original matrix 
##  - Contains a function setIM()to assign the inverse matrix of x to I
##  - Contains a function getIM() to obtain the cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    
    I <- NULL
    get <- function() x
    setIM <- function(IM) I <<- IM
    getIM <- function() I
    list(get=get, setIM=setIM, getIM=getIM)
}

## The "cacheSolve" function first performs a check to ascertain if the inverted
## martix has already been calculated and cached. If found, it is simly returned.
##If not, the calculation is made and the result cached and returned.

cacheSolve <- function(x) {
        
    I <- x$getIM()
    if(!is.null(I)){
        message("Getting cached data ...")
        return(I)
    }
    else {
        message("Calculating inverse matrix...")
        data <- x$get()
        I <- solve(data)
        x$setIM(I) 
        return(I)
    }
}
