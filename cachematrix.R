## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function, makeCacheMatrix, sets and gets a matrix, and caches it's inverse.

makeCacheMatrix <- function(x = matrix()) {
	## first, set a matrix
	## second, get the matrix
	## set the inverse of the matrix
	## get the inverse of the matrix

	inv = NULL
	set = function(y){
		x <<-y
		inv <<- NULL
	}

	get = function() x
	setinv = function(inverse) inv <<- inverse
	getinv = function() inv
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function
## the function cacheSolve, computes the inverse provided by  makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' -- from makeCacheMatrix above
		
		inv=x$getinv()
		
		##see if the inverse has already been calculated
		if (!is.null(inv)){
			##it already has been calculated, return the inverse
			return(inv)
		}

		## calculate inverse
		mat.data = x$get()
		inv = solve(mat.data, ...)
		
		## set value of inverse in the cacheSolve
		x$setinv(inv)
		
		return(inv)
}

