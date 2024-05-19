#############################################################################
###                      METODOS NUMERICOS
###           (formulas do Prof. Wagner Bonat UFPR DSBD)
#############################################################################

# INTEGRACAO NUMERICA ----

## Metodo de SIMPSON 1 de 3 ----
simpson <- function(integrando, a, b, ...){
  h <- (b-a)/2
  x2 <-(a+b)/2
  integral <- (h/3)*(integrando(a,...) +
                       4*integrando(x2, ...) + 
                       integrando(b, ...))
  return(integral)
}

### implemento
### funcao a integrar
fx <- function(x) x^2
### integrando
simpson(integrando = fx, a = 2, b = 3)


# QUADRATURA GAUSSIANA ---- 

## Gauss-Legendre, Gauss-Jacobi e Gauss-Cehbyshev ----
require(pracma)
gaussLegendre(n = 2, a = -1, b = 1)


gauss_legendre <- function(integrando, n.pontos, a, b, ...){
  pontos <- gaussLegendre(n.pontos, a = a, b = b)
  integral <- sum(pontos$w*integrando(pontos$x,...))
  return(integral)
}

## implementando (pontos de integração = mais precisao)
gauss_legendre(integrando = cos, n.pontos = 2, a = -1, b = 1)
gauss_legendre(integrando = cos, n.pontos = 10, a = -1, b = 1)

## Gauss-Laguerre ----
gauss_laguerre <- function(integrando, n.pontos, ...){
  pontos <- gaussLaguerre(n.pontos)
  integral <- sum(pontos$w*integrando(pontos$x,...)
                  /exp(-pontos$x))
  return(integral)
}

## implementando
fx <- function(x, lambda) lambda*exp(-lambda*x)

gauss_laguerre(integrando = fx, n.pontos = 2, lambda = 10)
gauss_laguerre(integrando = fx, n.pontos = 10, lambda = 10)
gauss_laguerre(integrando = fx, n.pontos = 100, lambda = 10)



## Gauss-Hermite ----
gauss_hermite <- function(integrando, n.pontos, ...) {
  pontos <- gaussHermite(n.pontos)
  integral <- sum(pontos$w*integrando(pontos$x,...)
                  /exp(-pontos$x^2))
  return(integral)
}


gauss_hermite(integrando = dnorm, n.pontos = 2)
gauss_hermite(integrando = dnorm, n.pontos = 10)
gauss_hermite(integrando = dnorm, n.pontos = 100)




# APROXIMACAO DE LAPLACE ----
laplace <- function(funcao, otimizador,n.dim, ...){
  integral <- -999999
  inicial <- rep(0,n.dim)
  temp <- try(optim(inicial, funcao,..., method=otimizador,
                    hessian=TRUE, control=list(fnscale=-1)))
  if(class(temp) != "try-error"){
    integral <- exp(temp$value) * (exp((n.dim/2)*log(2*pi) -
                                         0.5*determinant(-temp$hessian)$modulus))}
  return(integral)
}


laplace(dnorm, otimizador = "BFGS", n.dim = 1, log = TRUE)



# INTEGRACAO DE MONTE CARLO ----
monte.carlo <- function(funcao, n.pontos, ...) {
  pontos <- rnorm(n.pontos)
  norma <- dnorm(pontos)
  integral <- mean(funcao(pontos,...)/norma)
  return(integral)
}


## Integrando a Normal padrão
monte.carlo(funcao = dnorm, n.pontos = 1000)
## Integrando distribuição t com df = 30
monte.carlo(funcao = dt, n.pontos = 1000, df = 30)


# FUNCOES DO R PARA INTEGRACAO NUMERICA ----
args(integrate)

integrate(f = dnorm, lower = -Inf, upper = Inf)


fx <- function(x)x^2
integrate(f = fx, lower = -1, upper = 1)