## makeCacheMatrix -> Sets a matrix, gets it, then sets the inverse and gets the inverse

makeCacheMatrix <- function(x = matrix()) { ##setting the matrix
            m <- NULL
            set <- function(y){ 
                  x <<-y     ## sets x as the variable in the parent directory
                  m <<- NULL ## nulls previous values for the matrix
            }
            get <- function () x ## getter for matrix
            setinverse <- function (solve) 
                  m <<- solve  ##setter for inversed matrix
            getinverse <- function () m #getter for inversed matrix
            list( set = set, get=get, setmean=setmean, getmean=getmean)#writing as a list
            }

## caches (is this a verb?) the inverse of a matrix and solves it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)){message ("Getting cached data") #checks on previous fx if inverse exists
      return (m)      
      }
      data <-x$get() #retrieving from previous fx
      m <- solve(data,...) #finding the inverse of matrix
      x$setinverse(m)
      m #displays inverse
}
