## The combination of both functions allow the use of the inverse  
## of a given matrix priviously calculated, reducing the time 
## needed to recalculate the inverse of the same matrix. 

## makeCacheMatrix function create list object containing 
## basic functions to populate, read, populate the inverse and get the inverse of a matrix 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
	  x <<- y
	  m <<- NULL
	}
	get <- function() x
	setinv <- function (inverse) inv <<- inverse
	getinv <- function () inv
	list (set=set,get=get,setinv=setinv,getinv=getinv)
}


## cacheSolve function return the inverse of a matrix x from the cash 
## using the object created by makeCacheMatrix if it exists, otherwise 
## the inverse is calculated and cashed for future use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
        	message("getting cached data")
                return(inv)
        }
        d <- x$get()
        inv <- solve(d)
        x$setinv(inv)
        inv
}
