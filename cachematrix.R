##Coursera 10 - 24 - 2015
##The first function, makeCacheMatrix creates a special "matrix", which is 
##really a list containing a function to:
##1-set the value of the matrix
##2-get the value of the matrix
##3-set the value of the inverse
##4-get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
##Initialize the value of the mean as NULL
        InverseM <- NULL
##Defines the function set wich is responsible for setting a value for 
##the matrix. X and InverseM are defined in another environment, thtat´s why we 
##use the <<- Operator
        set <- function(y) {
                x <<- y
                InverseM <<- NULL
        }
##Defines the function get wich is responsible for returning the matrix X.
        get <- function()
                x
##Defines the function setInverse wich is responsible for setting the inverse
##of the matrix x into the variable InverseM. InverseM is defined in another 
## environment, that´s why we use the <<- operator.
        setInverse <- function(inverse)
                InverseM <<- inverse
##Defines the function getInverse wich is responsible for returning the 
##cached inverse of the matrix X.
        getInverse <- function()
                InverseM
##The output of this function is going to be a list with for elements. Each of 
##the elements are a function as explained in the Header
        list(
                set = set, get = get,
                setInverse = setInverse,
                getInverse = getInverse
        )
}

##The following function calculates the inverse of the special "matrix" created 
##with the above function. However, it first checks to see if the inverse has 
##already been calculated. If so, it gets the inverse from the cache and skips 
##the computation. Otherwise, it calculates the inverse of the data and sets the 
##value of the inverse in the cache via the set inverse function

cacheInverse <- function(x, ...) {
##First of all it reads the inverce of the matrix x
        inverse <- x$getInverse()
##If the inverse has already been calculated, the function returns it´s value
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
##If the inverse has not been calculated yet, the function reads the value of 
##the matrix, calculates the mean, than writes the inverse into the cache.
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}