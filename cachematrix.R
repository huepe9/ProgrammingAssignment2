## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
##
## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

## Función con get y set para regresar y modificar la amtriz

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function(){
                x
        }
        set_inversa <- function(inversa) {
                m <<- inversa
        }
        get_inversa <- function() {
                m
        }
        list(set = set, get = get, set_inversa = set_inversa, get_inversa = get_inversa)
        
}


## Funcion que regresa la matriz inversa si ya está calculada, de lo contrario 
## la realiza la función solve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inversa()
        if(!is.null(m)) {
                message("Recuperando el cache")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inversa(m)
        m
}
