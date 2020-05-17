makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
	x <<- y
	nv <<- NULL
}
	get = function() x
	setinv = function(inverse) inc <<- inverse
	getinv = function() inv
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
	inc = x$getinv()
	if (!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	mat.data = x$get ()
	inc = solve(mat.data, ...)
	x$setinv(inv)
	return(inv)
}
