#'
#' @title funcao linmod_class
#' @description Funcao que fixa a classe de retorno dos objetos "linmod".
#' @author Jonathan S. Matias
#' @docType package
#' @name print.linmod
#' @aliases print.linmod
#'
#'
NULL


#' @export
print.linmod <- function(x, ...){
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients)
}


#' @title sumario_princial
#' The parameter estimates of a statistical
#' @description Modelo sumarizado em 4 colunas contendo as estimativas, erro padrao, estatistica t e p-valor.
#' @author Jonathan S. Matias
#' @docType package
#' @name summary.linmod
#' @aliases summary.linmod
#'
NULL


#' @export
summary.linmod <- function(object, ...){
  se <- sqrt(diag(object$vcov))
  tval <- coef(object) / se
  TAB <- cbind(Estimate = coef(object),
               StdErr = se,
               t.value = tval,
               p.value = 2*pt(-abs(tval), df=object$df))
  res <- list(call=object$call,
              coefficients=TAB)
  class(res) <- "summary.linmod"
  res
}


#' @title Sumario_adicional
#' @description Funcao com objetivo de exibier uma matriz com melhor arredondamento e visualmente melhor.
#' @author Jonathan S. Matias
#' @docType package
#' @name print.summary
#' @aliases print.summary
#'
NULL

#' @export
print.summary.linmod <- function(x, ...){
  cat("Call:\n")
  print(x$call)
  cat("\n")
  printCoefmat(x$coefficients, p.value=TRUE, has.p.value=TRUE)
}
