#############################################################################
###                      METODOS NUMERICOS
###           (formulas do Prof. Wagner Bonat UFPR DSBD)
#############################################################################

# SISTEMAS DE EQUACOES NAO LINEARES

# Métodos numéricos para resolver equações são divididos em dois grupos:
# 1. Métodos de confinamento;
# 2. Métodos abertos.


# METODOS DE CONFINAMENTO ----

## BISSECAO ----
## algoritmo da bissecao
bissecao <- function(fx, a, b, tol = 1e-04, max_iter = 100) {
  fa <- fx(a); fb <- fx(b); if(fa*fb > 0) stop("Solução não está no intervalo")
  solucao <- c(); sol <- (a + b)/2; solucao[1] <- sol;
  limites <- matrix(NA, ncol = 2, nrow = max_iter)
  for(i in 1:max_iter) {
    test <- fx(a)*fx(sol)
    if(test < 0) {
      solucao[i+1] <- (a + sol)/2
      b = sol }
    if(test > 0) {
      solucao[i+1] <- (b + sol)/2
      a = sol }
    if( abs( (b-a)/2) < tol) break
    sol = solucao[i+1]
    limites[i,] <- c(a,b) }
  out <- list("Tentativas" = solucao, "Limites" = limites, "Raiz" = solucao[i+1])
  return(out)}

## implemento do argoritimo da bussecao
### funcao
ftheta <- function(theta){
  dd <- 2 * length(y) * (log(theta.hat/theta) + mean(y)*(theta - theta.hat) )
  return(dd - 3.84)
}

### Resolvendo numericamente
set.seed(123)
y <- rexp(20, rate = 1)
theta.hat <- 1/mean(y)
Ic_min <- bissecao(fx = ftheta, a = 0, b = theta.hat)
Ic_max <- bissecao(fx = ftheta, a = theta.hat, b = 3)
c(Ic_min$Raiz, Ic_max$Raiz) ## Solução aproximada





## REGULA FALSI ----
## algoritmo regula falsi
regula_falsi <- function(fx, a, b, tol = 1e-04, max_iter = 100) {
  fa <- fx(a); fb <- fx(b); if(fa*fb > 0) stop("Solução não está no intervalo")
  solucao <- c() ; sol <- (a*fx(b) - b*fx(a))/(fx(b) - fx(a))
  solucao[1] <- sol; limites <- matrix(NA, ncol = 2, nrow = max_iter)
  for(i in 1:max_iter) {
    test <- fx(a)*fx(sol)
    if(test < 0) {
      b = sol
      solucao[i+1] <- (a*fx(b) - b*fx(a))/(fx(b) - fx(a)) }
    if(test > 0) {
      a = sol
      solucao[i+1] <- sol <- (a*fx(b) - b*fx(a))/(fx(b) - fx(a)) }
    if( abs(solucao[i+1] - solucao[i]) < tol) break
    sol = solucao[i+1]
    limites[i,] <- c(a,b) }
  out <- list("Tentativas" = solucao, "Limites" = limites, "Raiz" = sol)
  return(out)
}

## implemento do algorimto regula falsi
### Resolvendo numericamente
Ic_min <- regula_falsi(fx = ftheta, a = 0.1, b = theta.hat)
Ic_max <- regula_falsi(fx = ftheta, a = theta.hat, b = 3)
### Solução aproximada
c(Ic_min$Raiz, Ic_max$Raiz)






# METODOS ABERTOS ----

## NEWTON ----
## algoritmo de Newton
newton_ma <- function(fx, f_prime, x1, tol = 1e-04, max_iter = 10) {
  solucao <- c()
  solucao[1] <- x1
  for(i in 1:max_iter) {
    solucao[i+1] = solucao[i] - fx(solucao[i])/f_prime(solucao[i])
    if( abs(solucao[i+1] - solucao[i]) < tol) break
  }
  return(solucao)
}

## implemento do algoritmo de Newton
### Derivada da função a ser resolvida
fprime <- function(theta){2*length(y)*(mean(y) - 1/theta)}
### Solução numerica
Ic_min <- newton_ma(fx = ftheta, f_prime = fprime, x1 = 0.1)
Ic_max <- newton_ma(fx = ftheta, f_prime = fprime, x1 = 2)
c(Ic_min[length(Ic_min)], Ic_max[length(Ic_max)])



## GRADIENTE DESCENDENTE ----
## algoritmo de gradiente descendente
grad_des_ma <- function(fx, x1, alpha, max_iter = 100, tol = 1e-04) {
  sol <- c()
  sol[1] <- x1
  for(i in 1:max_iter) {
    sol[i+1] <- sol[i] - alpha*fx(sol[i])
    if(abs(fx(sol[i+1])) < tol) break
  }
  return(sol)
}

## implemento do algotirmo de gradiente descendente
## Solução numerica
Ic_min <- grad_des_ma(fx = ftheta, alpha = -0.02, x1 = 0.1)
Ic_max <- grad_des_ma(fx = ftheta, alpha = 0.01, x1 = 4)
c(Ic_min[length(Ic_min)], Ic_max[length(Ic_max)])








