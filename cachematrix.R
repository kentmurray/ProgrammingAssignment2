## Kent Murray, 11/18/2014
 
## These functions create an object to store and retrieve
## an inverted matrix 

## makeCacheMatrix creates a special "matrix" object
## to hold four functions that 
## (1) set the value of the matrix, 
## (2) get the value of the matrix, 
## (3) store the inverted matrix , and 
## (4) retrieve the inverted matrix.

makeCacheMatrix <- function(x = matrix()) 
	{

    m <- NULL

	set <- function(y) 
		{
		x <<- y
		m <<- NULL
		}

	get <- function() 
		{ x }

	setinvert <- function(solve) 
		{ m <<- solve }

	getinvert <- function() 
		{ m }

	list(set = set, get = get, setinvert = setinvert, getinvert = getinvert)
	
	}


## cacheSolve inverts the matrix.
## First, it checks to see if the inverted matrix has already been cached.
## If it finds a cached version, it retreives it.
## If there's no cached version, the function
## inverts the matrix and caches it.

cacheSolve <- function(x, ...) 
	{
	
    ## Return a matrix that is the inverse of 'x'

    m <- x$getinvert()

	if(!is.null(m)) 
		{
		message("getting cached inverted matrix")
		return(m)
		}

	data <- x$get()
	m <- solve(data, ...)
	x$setinvert(m)
	m
		
	}
