## The first function, named makeCacheMatrix, creates a matrix that can cache its inverse.
## The first step is to initialized x ad a function argument, and set it as a matrix. 
## i s set to NULL, initializing it as an object within the makeCacheMatrix() environment to be used by later code in the function.

## Following, makeCacheMatrix() will define the 'getters and setters': 
## the first to be defined is set(), which takes an argument, named with any object name different from x, in this case y.
## When set() is executed, it assigns the input argument to the x object in the parent environment, and
## it assigns the value of NULL to the i object in the parent environment, thus clearing any value of i that had been cached by a prior execution of cacheSolve().
## In this way, when a new value of x is given, the cacheSolve() is cleared and it is forced to recalculate instead of giving the wrong return.
## We are using the <<- command to assign the value on the right side of the operator to an object in the parent environment named by the object on the left side of the operator.

## The second behavior that has to be defined in makeCacheMatrix() is get(), the getter for the matrix x.

## This step is followed by the setting of inverse, setinverse, thus defining the setter for the inverse i.

## Just like in the second step, now we need to define the getter for the inverse i.

## The following step will be to create a new object by creating a list() in which each element is named. This step will help us later on to extract the operator through the $.


## Overall, the first function is divided in 3 steps. 
## In the first part we need to initialize the objects, x (the matrix) and i (its inverse). 
## The second step is to define the 'setters and getters' for the objects x and i. 
## The last step is to create a new object by creating a list with each element named to help us with the extraction in the following steps.
## As a first step, makeCacheMatrix() is incomplete without cacheSolve(), because this latter will be the only place where the inverse() function can be executed. 


makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i 
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The second function, cacheSolve(), is needed to execute the inverse of the matrix, x.
## It starts with a single argument, x, and an ellipsis that allows us to pass additional arguments into the function.
## In the second step, the function needs to retrieve the inverse from the object passed in as the argument. 
## So, as the first thing to do, it calls the getinverse() function on the input object, x, and it associates it to i.
## Once it gets the inverse, we need to check whether the result is NULL, through !is.null() function.
## Since in makeCacheMatrix() we set the cached inverse, i, to NULL whenever a new matrix is set into the object, 
## if the value here is not equal to NULL, we have a valid, cached inverse value and can return it to the parent environment.
## In this case, which is we will obtain the message 'getting cached data' and then the inverse matrix.
## If the result of !is.null(i) is FALSE - as we have a NULL value, which is the case for a new matrix, x - cacheSolve() gets the matrix from the input object, x, 
## it calculates the inverse(), uses the setinverse() function on the input object, x, to set the inverse in the input object, 
## and then returns the value of the inversed matrix, i, to the parent environment by printing the inversed matrix, i. 

## As said above, this second function is needed since it is the only place where the inverse() function can be executed.
## The function will retrieve the inverse matrix of the input object, it associates it to i, then checks whether it is NULL or not.
## If it is not NULL (no new matrix), it will return cached data and the inverse matrix cached.
## If it is NULL (new matrix) it will run the functions to calculate the inverse of the new matrix.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message('getting cached data')
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i        ## Return a matrix that is the inverse of 'x'
}
