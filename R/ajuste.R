#'
#' @title funcao de ajuste do modelo
#' @description Funcoes que tem como objetivo ajusta o modelo com base nos Minimos Quadrados e estimar todos os parametros.
#' @author Jonathan S. Matias
#' @docType package
#' @name linmodEst
#' @aliases jonathan_ajuste
#' @examples
#' reg <- linmodEst(cbind(1, dados$x), dados$y)
#' reg
#'
NULL

#' @export
linmodEst <- function(x, y){
  ## compute QR-decomposition of x
  qx <- qr(x)
  ## compute (x’x)^(-1) x’y
  coef <- solve.qr(qx, y)
  ## degrees of freedom and standard deviation of residuals
  df <- nrow(x)-ncol(x)
  sigma2 <- sum((y - x%*%coef)^2)/df
  ## compute sigma^2 * (x’x)^-1
  vcov <- sigma2 * chol2inv(qx$qr)
  colnames(vcov) <- rownames(vcov) <- colnames(x)
  list(coefficients = coef,
       vcov = vcov,
       sigma = sqrt(sigma2),
       df = df)
}



#' @title limod_principal
#' @description Principal funcao generica limod
#' @author Jonathan S. Matias
#' @docType package
#' @name linmod
#' @aliases jonathan_ajuste1
#' @examples
#' reg2 <- linmod(cbind(1, dados$x), dados$y)
#' reg2
NULL

#' @export
linmod <- function(x, ...){
  UseMethod("linmod")
}

#' @title limod_defaut
#' @description Funcao limod para adicionar o metodo que estara disponivel no default.
#' @author Jonathan S. Matias
#' @docType package
#' @name linmod.defaut
#' @aliases jonathan_ajuste2
#'
NULL

#' @export
linmod.default <- function(x, y, ...){
  x <- as.matrix(x)
  y <- as.numeric(y)
  est <- linmodEst(x, y)
  est$fitted.values <- as.vector(x %*% est$coefficients)
  est$residuals <- y - est$fitted.values
  est$call <- match.call()
  class(est) <- "linmod"
  est
}

#' @title limod_formula
#' @description Funcao que tem como objetivo estabelecer a formula basica que deve ser utilizada na funcao a fim de estimar o modelo.
#' @author Jonathan S. Matias
#' @docType package
#' @name linmod.formula
#' @aliases jonathan_ajuste3
#'
NULL

#' @export
linmod.formula <- function(formula, data=list(), ...){
  mf <- model.frame(formula=formula, data=data)
  x <- model.matrix(attr(mf, "terms"), data=mf)
  y <- model.response(mf)
  est <- linmod.default(x, y, ...)
  est$call <- match.call()
  est$formula <- formula
  est
}
