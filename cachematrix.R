makeCacheMatrix <- function(x = matrix()) {
    invertm <- NULL
    set <- function(y) {
        
        #here i check if y is matrix
        if (is.matrix(y))
        {
            #here i check if x is empty or if value of X matrix were changed 
            result= tryCatch({sum(rowSums(x-y))},error=function(e){result = 1})
            if (result!=0)
            {
                print( "new matrix set")
                x <<- y
                invertm <<- NULL
            }
            else
            {
                print ("no new matrix added")
            }
        }
    }
    get <- function() x
    setinvert <- function(inv) invertm <<- inv
    getinvert <- function() invertm
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
}

cacheinvert <- function(x, ...) {
    inv <- x$getinvert()
    if(!is.null(inv)   ) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
         
    inv <- solve(data, ...)
    x$setinvert(inv)
    inv
}
