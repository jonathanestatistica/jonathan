% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ajuste.R
\docType{package}
\name{linmod}
\alias{linmod}
\title{limod_principal}
\description{
Principal funcao generica limod
}
\examples{
reg <- linmod(cbind(1, dados$x), dados$y)
summary(reg)
reg1 <- linmod(cbind(1, dados2$x, dados2$z, dados2$w), dados$y)
summary(reg1)
}
\author{
Jonathan S. Matias
}
