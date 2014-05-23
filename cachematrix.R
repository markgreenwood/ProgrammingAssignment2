## makeCacheMatrix creates a special matrix object that can cache its inverse using cacheSolve().
##
## Example:
##
## >>> M <- makeCacheMatrix(matrix(c(1,2,3,2,1,2,3,2,1),c(3,3)))
## >>> cacheSolve(M) # returns the computed value of the inverse
## ...
## ...do some other stuff that leaves M unchanged...
## ...
## >>> cacheSolve(M) # returns the cached value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        # Default: no inverse computed/cached yet upon creation of CacheMatrix
        xinv <- NULL
        
        # set() method assigns argument y to the CacheMatrix's internal matrix
        #       representation x; the only way to see what's contained in x is
        #       via the get() method.
        set <- function(y) {
                x <<- y
                # whenever the matrix is "set", xinv gets NULL so cacheSolve
                #       is forced to recompute the inverse
                xinv <<- NULL 
        }
        
        # get() method returns CacheMatrix's internal matrix representation x;
        #       the only way to change x is via the set() method.
        get <- function() x
        
        # The setinv() method assigns argument minv to CacheMatrix's internal
        #       inverse representation xinv
        setinv <- function(minv) xinv <<- minv
        
        # The getinv() method returns CacheMatrix's internal inverse
        #       representation xinv
        getinv <- function() xinv
        
        # Finally, makeCacheMatrix returns a list of methods used
        #       to access the internals of the created object; this controls
        #       access to the object by limiting the interface to the defined
        #       methods
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve returns the inverse of the CacheMatrix x.
## If the inverse has previously been cached and the matrix has not changed
##      (via the set() method), it returns the cached value.
## If the inverse has not been cached or the matrix has changed
##      (via the set() method), it calculates the inverse, caches it, and returns it.
## Note that additional arguments to the solve() function can be passed to cacheSolve()
##      in addition to the CacheMatrix argument x.

cacheSolve <- function(x, ...) {
        
        # Get the internal representation of the inverse
        minv <- x$getinv()
        
        # If the stored inverse is not NULL, you can use the cached value
        if (!is.null(minv)) {
                message("getting cached inverse")
                return(minv)
        }
        
        # Otherwise, you have to get the representation of the matrix,
        #       calculate the inverse using solve(), store the calculated
        #       inverse in the cache, and return it
        m <- x$get()
        minv <- solve(m, ...)
        x$setinv(minv)
        minv
}
