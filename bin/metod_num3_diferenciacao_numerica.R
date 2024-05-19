#############################################################################
###                      METODOS NUMERICOS
###           (formulas do Prof. Wagner Bonat UFPR DSBD)
#############################################################################

# DIFERENCIACAO NUMERICA ----

## diferenca progressiva ----
dif_prog <- function(fx, x, h) {
  df <- (fx(x + h) - fx(x))/( (x + h) - x)
  return(df)
}

## diferenca regressiva ----
dif_reg <- function(fx, x, h) {
  df <- (fx(x) - fx(x - h))/( x - (x - h))
  return(df)
}

## diferenca central ----
dif_cen1 <- function(fx, x, h) {
  df <- (fx(x + h) - fx(x - h))/( (x + h) - (x - h))
  return(df)}

## implemento das diferencas
### aproximacao de derivadas por diferencas finitas
fx <- function(x) x^3
deriv_fx <- 3*2^2

dif_prog(fx, x = 2, h = 0.001) ## Diferença progressiva
dif_reg(fx, x = 2, h = 0.001) ## Diferença regressiva
dif_cen(fx, x = 2, h = 0.001) ## Diferença central



# DIFERENCIAS FINITAS USANDO EXPANSAO EM SERIES DE TAYLOR -----


# EXTRAPOLAÇÃO DE RICHARDSON ----
fx <- function(x) (2^x)/x

# exata
dervi_fx <- function(x)(log(2)*(2^x)) / x - (2^x)/x^2


# diferenca central
dif_cen(fx = fx, x = 2, h = 0.2)
# erro em relacao a exata
erro <- dervi_fx(x = 2) / dif_cen(fx = fx, x = 2, h = 0.2)
(erro-1)*100

# Richardson
D2 <- dif_cen(fx = fx, x = 2, h = 0.2/2)
D <- dif_cen(fx = fx, x = 2, h = 0.2)
der <- (1/3)*( 4*D2 - D)
der
## erro em relacao a exata
erro2 <- dervi_fx(x = 2)/der
(erro2-1)*100


c("Exata" = dervi_fx(x = 2), "Richardson" = der,
  "Central" = dif_cen(fx = fx, x = 2, h = 0.2))


# DERIVADAS PARCIAIS ----
# formula dois pontos central
dif_cen2 <- function(fx, pt, h, ...) {
  df <- (fx(pt + h, ...) - fx(pt - h, ...))/( (pt + h) - (pt - h))
  return(df)
}

# formula do gradiente numerico (usa a formula dos dois pontos centrais acima)
grad_fx <- function(fx, par, h, ...) {
  fbeta0 <- function(beta0, beta1, y, x) fx(par = c(beta0, beta1), y = y, x = x)
  fbeta1 <- function(beta1, beta0, y, x) fx(par = c(beta0, beta1), y = y, x = x)
  db0 <- dif_cen2(fx = fbeta0, pt = par[1], h = h, beta1 = par[2], y = y, x = x)
  db1 <- dif_cen2(fx = fbeta1, pt = par[2], h = h, beta0 = par[1], y = y, x = x)
  return(c(db0, db1))
}

# implementacao
fx <- function(par, y, x1) {sum ( abs( y - (par[1] + par[2]*x1)) )}



# simulando y e x
set.seed(123)
x <- runif(100)
y <- rnorm(100, mean = 2 + 3*x, sd = 1)

# gradiente numerico
grad_fx(fx = fx, par = c(2, 3), h = 0.001, y = y, x1 = x)

# gradiente analitico
c(sum(((y - 2 - 3*x)/abs(y - 2 -3*x))*(-1)),
  sum(((y - 2 - 3*x)/abs(y - 2 -3*x))*(-x)))



# FUNCOES DO R PARA DIFERENCIACAO NUMERICA ----
require(numDeriv)
args(grad)
args(hessian)

# aplicação
grad(func = fx, x = c(2, 3), y = y, x1 = x)
hessian(func = fx, x = c(2, 3), y = y, x1 = x)

