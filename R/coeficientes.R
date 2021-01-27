
#' @title Coeficientes do modelo estimado
#' @description Funcao para pegar os coeficientes estimados no modelo linear de regressao - linmod.
#' @author Jonathan S. Matias
#' @docType package
#' @name coeficientes
#' @aliases coeficientes
#' @examples
#' x = cbind(1, dados2$x, dados2$z,dados2$w)
#' y = dados2$y
#' coeficientes(x,y)
NULL

#' @export
coeficientes <- function(x, y, ...){
  x <- as.matrix(x)
  y <- as.numeric(y)
  est <- linmodEst(x, y)
  est$fitted.values <- as.vector(x %*% est$coefficients)
  class(est) <- "linmod"
  est
}
