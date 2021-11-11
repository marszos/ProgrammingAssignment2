

makeCacheMatrix <- function(x = matrix()) {


  # intialization of two objects, x (matrix _ default value: empty two dimensional data structure) and mat_inv (the inverse of the matrix)
  #  x is initialized as a function argument. The inverse is set to NULL.
  mat_inv <- NULL
  # after intialization the following code provides object-oriented style methods for setting and retrieving the data within an object.
  # set() takes an argument y, it is assumed that it's value is matrix. Set() uses <<- form of the assignment operator.
  # If you use the assignment operator <<- the variable belongs to the global scope.

  set <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }

  # get function retrieves x object (matrix) from the parent environment of makeCacheMatrix()
  get <- function() x
  # since mat_inv is defined in the parent environment and we need to access it after set_inv() completes, the code uses the <<- form
  # of the assignment operator to assign the input argument to the value of mat_inv in the parent env.
  set_inv <- function(inverse) mat_inv <<- inverse
  # get_inv() via lexical scoping finds mat_inv variable and retrieves its value
  get_inv <- function() mat_inv
  # the output of the the functions above is stored as an element of in the list and it is returned to the parent environment.
  # Each element of the list is named what allows us to use the $ form of the extract operator to access the function by name.
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}


# cacheSolve() requires an input argument of type makeCacheMatrix().
# execution of cacheSolve() function will populate and retrieve the inverse of the matrix for an obejct of type makeCacheMatrix()

cacheSolve <- function(x, ...) {

# function attemps to retrieve the inverse of the matrix value from the object passed in as the argument.
  # It calls get_inv() function on the input object.
  mat_inv <- x$get_inv()
  # checking if mat_inv is NULL. If mat_inv is NOT equal to NULL we have a valid, cached value for the inverse of the matrix which can
  # be returned to the parent environment.

  if(!is.null(mat_inv)) {
    message("getting cached data")
    return(mat_inv)
  }
  # if the result of !is.null(mat_inv) is FALSE, cacheSolve() gets the matrix object and calculates inverse value using solve() method
  # than using set_inv() function sets the inverse value in the input object and returns the inverse value of the matrix by printing it.
  data <- x$get()
  mat_inv <- solve(data, ...)
  x$set_inv(mat_inv)
  mat_inv
}
