#'
#' @title Coeficientes
#' @description Funcao que ira fazer o print dos coeficientes do modelo estimado usando a funcao linmod.
#' @author Jonathan S. Matias
#' @docType package
#' @name print.linmod
#' @aliases print.linmod
#'#' @examples
#' x = cbind(1, dados2$x, dados2$z,dados2$w)
#' y = dados2$y
#' mod = linmod(x,y,z,w)
#' print(mod)
#'
NULL

#' @export
print.linmod <- function(x, ...){
  cat("Call:\n")
  print(x$call)
  cat("\nCoefficients:\n")
  print(x$coefficients)
}


#' @title Sumario
#' @description Modelo sumarizado em 4 colunas contendo as estimativas, erro padrao, estatistica t e p-valor.
#' @author Jonathan S. Matias
#' @docType package
#' @name sumario
#' @aliases sumario
#' @examples
#' x = cbind(1, dados2$x, dados2$z,dados2$w)
#' y = dados2$y
#' mod = linmod(x,y,z,w)
#' summary(mod)
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

print.summary.linmod <- function(x, ...){
  cat("Call:\n")
  print(x$call)
  cat("\n")
  printCoefmat(x$coefficients, p.value=TRUE, has.p.value=TRUE)
}
