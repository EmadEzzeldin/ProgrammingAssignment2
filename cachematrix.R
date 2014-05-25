makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {##First Function: Takes I/P , assigns the argument of head functions to this I/P
    x <<- y
    m <<- NULL
  }
  get <- function() x##Second function....EMPTY FUNCTION RETURNING VARIABLE X
  list(function1 = set, function2 = get  ## list('a'=1,'b')=2 .... $a [1]1
  ) 
}

m <- matrix(1:4, nrow = 2, ncol = 2)
makeCacheMatrix(m)
makeCacheMatrix()$function2()
makeCacheMatrix()$function1(m)
makeCacheMatrix()$function2()
#TESTING FUNCTION 1
# > makeVector(100)$get()
# [1] 100
# > makeVector()$get()
# numeric(0)
# > 

# TESTING FUNCTION 2



# > makeCacheMatrix(m)$function2()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > makeCacheMatrix()$function2()
# [,1]
# [1,]   NA
# > 

