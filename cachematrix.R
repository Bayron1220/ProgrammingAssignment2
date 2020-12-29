##A continuación se muestran un par de funciones que almacenan en caché la inversa de una matrix siempre qwue sea reversible
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Aqui la solucion

cacheSolve <- function(x, ...) {
  inv  <-  x $ getInverse ()
  if ( ! is.null ( inv )) {
    volver ( inv )
  }
  mat  <-  x $ obtener ()
  inv  <- resolver ( mat , ... )
  x $ setInverse ( inv )
  inv
  }
