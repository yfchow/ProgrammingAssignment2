
## Here is the homework of "Assignment 2" finished by Yifan Zhou 
## Good practice of OOP Porgramming Concept

## This function is used to create "Objects". So the funcationality of this function 
## can be resued by many objects. (Quite a Clever Idea!!)

makeCacheMatrix <- function(x = matrix()) {
        Inver <- NULL
        ## Inverse matrix is initially set to NULL
        set <- function(y){
                x <<- y
                Inver <<- NULL
        }
        ## function to rewrite the original matrix
        get <- function() x
        ## function to read the matrix
        setinverse <- function(Inverse) Inver <<- Inverse
        ## function to rewirte the inverse of original matrix (computed outside)
        getinverse <- function() Inver
        ## function to read the inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)   
        ## Return a list containing the corresponding set/get functions for reuse
}


## Below is the main function to call the object created by previous function

cacheSolve <- function(x, ...) {
        Inver <- x$getinverse()
        ## read the inverse to check whether it is already computed
        if(!is.null(Inver)){
                message("getting cached data")
                return(Inver)
                ## if computed, just return it
        }
        data <- x$get()
        ## if not, read the orinila matrix
        Inver <- solve(data,...)
        ## and then compute it
        x$setinverse(Inver)
        ## store it for future reuse
        Inver
        ## Return a matrix that is the inverse of 'x'
}
