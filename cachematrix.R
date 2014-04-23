##makeCacheMatrix() and cacheSolve() work to calculate the inverse of a matrix 
#and store it in a cache.
#Call x <- makeCacheMatrix() to initialise a matrix cache
#Then call x$set(your-matrix-here) to load a matrix
#Finally, cacheSolve(x) will take the matrix from x, calculate the inverse with 
#solve(), cache it in x alongside the original matrix, and return the inverse. 
#If cacheSolve has been called previously, it will return the cached inverse
#from x.


##makeCacheMatrix()
#makeCacheMatrix() creates a list with 4 functions, which also stores a matrix 
#and its inverse
#makeCacheMatrix$set(providedMatrix) - loads a matrix into the makeCacheMatrix 
#environment
#makeCacheMatrix$get() - returns the matrix from this environment
#makeCacheMatrix$setInverse(providedInverse) - loads the inverse into the 
#makeCacheMatrix environment
#makeCacheMatrix$getInverse() - returns the inverse from this environment

makeCacheMatrix <- function(originalMatrix = matrix()) {
    inverseMatrix <- NULL
    
    #set loads the provided matrix into the cache and resets the cached inverse 
    #matrix
    set <- function(providedMatrix) {
        originalMatrix <<- providedMatrix
        inverseMatrix <<- NULL
    }
    
    #get returns the original matrix from the cache
    get <- function() {
        return(originalMatrix)
    }
    
    #setInverse loads the provided inverse into the cache
    setInverse <- function(providedInverse) {
        inverseMatrix <<- providedInverse
    }
    
    #getInverse returns the cached inverse
    getInverse <- function() {
        return(inverseMatrix)
    }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve()
#cacheSolve takes a list created with makeCacheMatrix which contains cached 
#values, plus getter and setter functions
#cacheSolve returns the inverse of the matrix contained in the list, either 
#from the cache or by solve
#the ... allows additional arguments to be passed to solve()

cacheSolve <- function(cacheList, ...) {
    #Check the cache
    cachedInverse <- cacheList$getInverse()
    
    if(!is.null(cachedInverse)) {
        #If there's an inverse in the cache, return it
        message("Getting cached data")
        return(cachedInverse)
    }
    else {
        #If no inverse in cache, calculate inverse, set it in cache, return 
        #calculated value
        message("Calculating inverse, setting cache")
        cachedMatrix <- cacheList$get()
        calculatedInverse <- solve(cachedMatrix, ...)
        cacheList$setInverse(calculatedInverse)
        return(calculatedInverse)
    }
}
