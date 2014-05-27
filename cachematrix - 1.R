## Due to failture to push the first copy of the assignment correctly, I ##have pushed this one by todays date. While keeping the old assignment ##untouched. It can be verified that I made no change to the submitted ##assignment since deadline.

makeVector <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  invert <- function(x) m <<- solve(x)
  getinverted <- function() m
  list(set = set, get = get,
       Invert = invert,
       getinverted = getinverted)
}

cachesolve <- function(x, ...) {
  m <- x$getinverted()
  if(!is.null(m)) {
    message("inverting matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$invert(m)
  m
}
