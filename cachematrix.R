## makeCacheMatrix() converts a regular matrix objected into a cache-able matrix.
## cacheSolve() makes the solve() routine cache-able for a particular object



makeCacheMatrix <- function(x = matrix()) {
 
 invs <- NULL
        set <- function(y)
        {
                x <<- y
                invs <<- NULL
        }
        
        get <- function() x
        
        setInv <- function(inv) invs <- inv
        getInv <- function() invs
        list(set = set, get = get, setInv=setInv, getInv=getInv)  
}




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		inv = x$getInv()
        if(!is.null(inv))
        {
                return(inv)
        }
        data <- x$get()
        inv <-solve(data)
        x$setInv(inv)
        inv
}
