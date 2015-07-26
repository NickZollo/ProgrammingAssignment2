## These two functions save time inversing a matrix by caching the result, therefore 
## ensuring that the computation is not performed repeatedly for the same matrix.

## This function creates a list of 4 functions to cache a matrix and its inverse:
## set()		-sets the value of the matrix
## get()		-returns the value of the matrix
## setInverse()		-sets the value of the inverse
## getInverse() 	-returns the value of the inverse

makeCacheMatrix <- function(x = matrix()) 
{
	inversed <- NULL 				## marked as NULL until the inverse is computed

	set <- function(y) 			## sets the value of the matrix, then stores the new value
	{
		x <<- y
		inversed <<- NULL 		## acts as a flag showing that the inverse isn't computed yet
	}

	get <- function() 			## returns the stored matrix
	{ 
		x 
	}

	setInverse <- function(inverse) 	## caches the inverse of matrix after it is computed
	{
		inversed <<- inverse 		## flag indicates that the inverse has been computed
	}
	
	getInverse <- function() 		## returns the cached inverse
	{ 
		inversed 
	}

	## returns a list of the accessor functions for the matrix
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns the inverse of matrix x, if the inverse is already cached thorugh the above function,
## it will skip the computation by retrieving the cached inverse, and if the inverse is not cached,
## it will compute the inverse and cache the result.

cacheSolve <- function(x, ...) 
{
	inversed <- x$getInverse() 		## retrieves the cached inverse, whether or not it has been computed

	if(!is.null(inversed)) 			## checks if the inverse has been computed yet and returns it if so
	{
		message("Getting cached data...")
		inversed
	}
	else ## if the inverse has not yet been computed, computes and caches it
	{
		matrix <- x$get()			## accesses the matrix
		inversed <- solve(matrix)	## computes the inverse of the matrix
		x$setInverse(inversed) 		## stores the inverse of the matrix
		inversed				## returns the inverse of the matrix
	}		
}
