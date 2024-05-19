#############################################################################
###                      METODOS NUMERICOS
###           (formulas do Prof. Wagner Bonat UFPR DSBD)
#############################################################################

# SISTEMAS DE EQUACOES NAO LINEARES

# Métodos numéricos para resolver equações são divididos em dois grupos:
# 1. Métodos de confinamento;
# 2. Métodos abertos.

# SISTEMAS DE EQUACOES ----

## METODO DE NEWTON ----
## algoritmo de Newton
newton_sa <- function(fx, jacobian, x1, tol = 1e-04, max_iter = 10) {
  solucao <- matrix(NA, ncol = length(x1), nrow = max_iter)
  solucao[1,] <- x1
  for(i in 1:max_iter) {
    J <- jacobian(solucao[i,])
    grad <- fx(solucao[i,])
    solucao[i+1,] = solucao[i,] - solve(J, grad)
    if( sum(abs(solucao[i+1,] - solucao[i,])) < tol) break
  }
  return(solucao)
}

## implemento do algoritimo de Newton

## Sistema a ser resolvido
fx <- function(x){
  c(x[2] - 0.5*(exp(x[1]/2) + exp(-x[1]/2)),9*x[1]^2 + 25*x[2]^2 - 225 )
}

## Jacobiano
Jacobian <- function(x) {
  jac <- matrix(NA,2,2)
  jac[1,1] <- -0.5*(exp(x[1]/2)/2 - exp(-x[1]/2)/2)
  jac[1,2] <- 1
  jac[2,1] <- 18*x[1]
  jac[2,2] <- 50*x[2]
  return(jac)
}

## Resolvendo
sol <- newton_sa(fx = fx, jacobian = Jacobian, x1 = c(1,1))
### solucao
tail(sol,4)
## check if ok
fx(sol[8,])


## METODO GRADIENTE DESCENDENTE ----
## algoritmo de gradiente descentente
grad_des_sa <- function(fx, x1, alpha, max_iter = 100, tol = 1e-04) {
  solucao <- matrix(NA, ncol = length(x1), nrow = max_iter)
  solucao[1,] <- x1
  for(i in 1:c(max_iter-1)) {
    solucao[i+1,] <- solucao[i,] - alpha*fx(solucao[i,])
    #print(c(i, solucao[i+1,]))
    if( sum(abs(solucao[i+1,] - solucao[i,])) <= tol) break
  }
  return(solucao)
}


## implemento do algoritmo de gradiente descendente
fx <- function(x) {
  y <- c(5.15, 6.40, 2.77, 5.72, 6.25, 3.45, 5.00, 6.86, 4.86, 3.72)
  z <- c(0.28, 0.78, 0.40, 0.88, 0.94, 0.04, 0.52, 0.89, 0.55, 0.45)
  term1 <- - 2*sum(y - x[1] - x[2]*z)
  term2 <- -2*sum( (y - x[1] - x[2]*z)*z)
  out <- c(term1, term2)
  return(out)
}

sol_grad <- grad_des_sa(fx = fx, x1 = c(5, 0), alpha = 0.05, max_iter = 140)
fx(x = sol_grad[137,])

