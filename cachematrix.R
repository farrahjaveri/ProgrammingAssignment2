## Put comments here that give an overall description of what your
## functions do

## to be able to run cacheSolve() we need makeCacheMatrix()to return an argument
##that cacheSolve()can then use. makeCacheMatrix will be the environment of the
##object meaning that cacheSolve()can find the inverse of this special "matrix"


makeCacheMatrix <- function(x = matrix()) { ##objects invr and x needd to be 
        ##initialised, where invr is the matrix and x will be the argument
        invr <- NULL ##assigns inverse as NULL
        set <- function(z) { ## names function(z) as set where "z" is the matrix
        x <<- z ##using <<- allows us to assign z to x and NULL to inverse 
        ##within this secondary function, which is an environment different from 
        ##the current environment
        ## when we execute set, the input argument will be assigned to x, in the
        ##parent environment 
        invr <<- NULL ##this ensures that invr is assigned the value of NULL in
        ##the parent environment and clears any previous value of invr which
        ##could have arisen by running cacheSolve() beforehand
}
get <- function() x
setinverse <- function(inverse) invr <<- inverse ##setter for inverse of 
##invr
getinverse <- function() invr ##getter for inverse of invr
list(set=set, get=get, setinverse=setinverse, getinverse = getinverse)
##return a list where each function is an element in the list and feeds back to
##the parent environment, meaning $ can be used to access the function by name
}







## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) { ##funcrion retrieves inverse from matrix object
        ##that is made to be the argument
        invr <= x$getinverse() ##getinverse() applied to the input object
        if(!is.null(invr)) { ##checks to see if the object, "invr" results in
                ##NULL and since makeCacheMatrix()ensures that the inverse cache 
                ##of any new matrix is NULL, any result that is not NULL gives
                ##a valid cached inverse to be returned to parent environment
                message("Getting Cached Data:-")
                return(invr)
        } ##if the result of !is.null(invr) is FALSE, cacheSolve() takes the
        ##matrix from the input object and applied solve()to it, followed by
        ## setinverse() to set the inverse of the object, giving the value of
        ##the inverse in the parent environment, prints inverse of object
        matrx <- x$get()
        invr <- solve(matrx, ...)
        x$setinverse(invr)
        invr
}
       

