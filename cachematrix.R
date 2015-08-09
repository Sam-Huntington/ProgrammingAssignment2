## SUMMARY
## makeCacheMatrix takes a matrix as input and returns a list
##      with functions to set and get the matrix, and to set and  
##      get the inverse of the matrix. This severs as the input to
##      cacheSolve()
## cacheSolve takes the list output of makeCacheMatrix and returns
##      the inverse of the original matrix. if the inverse has already   
##      been calculated it retrieves it from the cache, and if not it
##      calculates it on the spot


makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y){
		x <<- y
		i <<- NULL
	}
	get = function() x
	setinverse = function(inv) i <<- inv
	getinverse = function() i
	list( set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	
	i = x$getinverse()

	## if inverse already exists in cache, retrieve it
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}
	
	## if not, calculate the inverse, cache it, and return it
	matr = x$get()
	i <- solve(matr,...)
	x$setinverse(i)
	return(i)

}
