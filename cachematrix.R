## Matrix Inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.

## The function below creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv_m <- NULL
	set <- function(y){
		x <<- y
		inv_m <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv_m <<- inverse
	getInverse <- function() inv_m
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

## If the inverse has already been calculated and the matrix hasn't changed, then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_m <- x$getInverse()
        if(!is.null(inv_m)){
        	message("getting cached data")
        	return(inv_m)
        }
        matrx <- x$get()
        inv_m <- solve(matrx, ...)
        x$setInverse(inv_m)
        inv_m
}
