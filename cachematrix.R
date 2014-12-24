
## makeCacheMatrix creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL

	## set new value
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}

	## get value from memory
	get <- function() x

	## set matrix inverse
	setinv <- function(inverse) inv <<- inverse

	## get matrix inverse
	getinv <- function() inv

	## generate list
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve is to calculates the inverse of the special "matrix" 
## created with the above function.  If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()

	if(!is.null(inv)) {
		message("getting cached data")
		return (inv)
	}

	data <- x$get()

	if(det(data) == 0) {
		message("not invertible")
		inv <- "NA"
	} else {
		inv <- solve(data)
	}
	x$setinv(inv)
	inv
}
