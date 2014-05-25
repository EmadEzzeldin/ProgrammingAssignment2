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
