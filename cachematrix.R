
## This function contains four nested functions, which save the input matrix/its 
## inverse in the cache (in the parent environment),and return these values when called

makeCacheMatrix <- function(x=matrix()) { #function with a matrix as argument
        inv <- NULL #sets inverse object as NULL to eventually store cached inverse
        
        set <- function(y) { #updates the matrix and clears cached inverse 
                x <<- y #sets the matrix in the parent environment
                inv <<- NULL #resets inverse to NULL since the matrix has been changed
        }
        get <- function() x #retrieves current matrix x
        
        setinverse <- function(inverse) inv <<- inverse #takes value (in this case the computed inverse of x) 
                                                        #and stores it in cache
        getinverse <- function () inv #retrieves cached inverse (returns NULL if no inverse has been computed yet)
        
        list(set=set,get=get, #returns a list with all four functions so they can be accessed later
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function checks whether the inverse of x has been computed before. 
## If it has, it returns the cached inverse;if not,it computes it and stores it.

cacheSolve <- function(x,...) { #function that takes matrix x as argument
        inv <- x$getinverse() #retrieves the cached inverse (if available) and stores it in the local variable inv
        
        if(!is.null(inv)) { #if there is an available cached inverse, it returns it
                message("getting cached data")
                return(inv)
        } 
        #if cached inverse is NULL (not yet computed):
        data <- x$get #calls the "get" function to retrieve x and stores it in data
        inv <- solve(data,...) #computes inverse of data and stores it in local inv object
        x$setinverse(inv) #stores the new inverse in the cache
        inv #returns computed inverse
}
