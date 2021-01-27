#'
#' @title Funcao para calcular os intervalos de confianca dos parametros
#' @description Esta funcao calcula o intervalo de confianca em torno nos parametros estimados considerando o nivel de confianca dado.
#' @docType package
#' @name interconf
#' @aliases jonathan_interconf
#'
#'
NULL

#' @export
interconf = function(x,y,t)
{
library(Jonathan)
x <- as.matrix(x)
y <- as.numeric(y)
vcov=vcov(x,y)
diag=vcov[col(vcov)==row(vcov)]
diag=as.matrix(diag)
mod2 = linmod(x,y)
mod2=as.matrix(mod2$coefficients)
p = qt(t, df = nrow(x))
p=as.matrix(rep(p,nrow(mod2)))
LI=mod2-p*diag
LS=mod2+p*diag
conf = cbind(LI, mod2, LS)
colnames(conf) <- c("LI", "coef", "LS")
conf

}
