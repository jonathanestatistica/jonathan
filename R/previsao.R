#'
#' @title funcao de previsao
#' @description Funcao que tem como objetivo calcular as previsoes baseadas no modelo ajustado.
#' @author Jonathan S. Matias
#' @docType package
#' @name previsao
#' @aliases previsao
#'
#'
NULL

#' @export
predict.linmod <- function(object, newdata=NULL, ...){
  if(is.null(newdata))
    y <- fitted(object)
  else{
    if(!is.null(object$formula)){
      ## model has been fitted using formula interface
      x <- model.matrix(object$formula, newdata)
    }
    else{
      x <- newdata
    }
    y <- as.vector(x %*% coef(object))
  }
  y
}
