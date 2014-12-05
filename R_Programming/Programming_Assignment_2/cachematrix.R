##In this R script we create two functions that will allow us to cache the value of the inverse of a given matrix.

## makeCacheMatrix takes a matrix x in argument and returns an object of type list. 
## It stores the value of the matrix x and the value of its inverse inv. 
## It returns a list containing 4 functions allowing us to get or set the values of x and inv.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                                        #we initialize the inverse to NULL at every call of makeCacheMatrix
        set <- function(y) {                               
                x <<- y                                    #set the value of x
                inv <<- NULL                               #as a new object x has been created, the value of inv has to be reset to NULL
        }
        get <- function() x                                #return the value of the matrix x
        setinverse <- function(inverse) inv <<- inverse    #store the value of the inverse in cache
        getinverse <- function() inv                       #return the value of the inverse stored in the cache
        
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)                      #a list is returned
}



## cacheSolve() takes the object returned by makeCacheMatrix, then it checks if the inverse has already
## been calculated and stored in the cache, in which case it returns the cached value. 
## Otherwise, it calculates the inverse. It stores the inverse in cache and retuns it.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()                         #we get the value of the inverse from the object x
        if(!is.null(inv)) {                           #if the inverse is not NULL then we get it from cache
                message("getting cached data")
                return(inv)                           #the inverse stored in cache is returned
        }                                        
        data <- x$get()                               #the inverse is not cached, we get the value of the matrix
        inv <- solve(data, ...)                       #the inverse is calculated
        x$setinverse(inv)                             #the value of the inverse is stored in cache
        inv                                           #the inverse is returned                                                       
}


