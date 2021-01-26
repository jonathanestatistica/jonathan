#'
#' @title Funcao variancia_covariancias
#' @description Funcao que tem como objetivo estimar a matriz de variancia e covariancias do modelo com base no linmod
#' @docType package
#' @name vcov
#' @aliases jonathan_vcov
#' @examples
#' x = cbind(1, dados2$x, dados2$z,dados2$w)
#' y = dados$y
#' vcov(x,y)
#'
NULL

#' @export
vcov <- function(x,y,...)
{
  x <- as.matrix(x)
  y <- as.numeric(y)

  est <- linmodEst(x,y)

  vcov <- as.matrix(est$vcov)

  vcov

}
