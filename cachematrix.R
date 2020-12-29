##A continuación se muestran un par de funciones que almacenan en caché la inversa de una matrix siempre qwue sea reversible
makeCacheMatrix <- function(x = matrix()) {
  
  ##Se iicia el NUll
  inv <- NULL
  
  ##Se selecciona la matriz
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  ##Aqui es donde se invierte la matrix
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Aqui la solucion donde se calcula la matriz inversa

cacheSolve <- function(x, ...) {
  inv  <-  x $ getInverse ()
  if ( ! is.null ( inv )) {
    volver ( inv )
  }
  ##Aqui se obtiene la matriz inversa
  mat  <-  x $ obtener ()
  inv  <- resolver ( mat , ... )
  ##Se seleccina la matriz
  x $ setInverse ( inv )
  ##Retorna la matriz
  inv
  }
