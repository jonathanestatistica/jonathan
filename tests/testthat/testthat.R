library(Jonathan)
data(dados)
data(dados2)


mod1 <- linmodEst(cbind(1,dados$x),dados$y)
mod1

mod2 <- linmod(cbind(1,dados$x),dados$y)
mod2

x = cbind(1, dados2$x, dados2$z,dados2$w)
y = dados2$y
coef <- coeficientes(x,y)
coef

conf <- interconf(cbind(1,dados2$x,dados2$y,dados2$z,dados2$w),dados2$y,.0975)
conf

x = cbind(1, dados2$x, dados2$z,dados2$w)
y = dados2$y
mod = linmod(x,y,z,w)
predict(mod)

x = cbind(1, dados2$x, dados2$z,dados2$w)
y = dados2$y
mod = linmod(x,y,z,w)
summary(mod)


x = cbind(1, dados2$x, dados2$z,dados2$w)
y = dados$y
vcov(x,y)



