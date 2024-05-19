#############################################################################
###                      METODOS NUMERICOS
###           (formulas do Prof. Wagner Bonat UFPR DSBD)
#############################################################################

# OTIMIZACAO

# Otimizando funcoes perda ----

## Passo 1 : implementar a funçao objetivo ----

# Perda quadrática
perda_quad <- function(mu, dd) { sum((dd-mu)^2) }
# Perda absoluta
perda_abs <- function(mu, dd) { sum(abs(dd-mu)) }
# Perda minimax
perda_minimax <- function(mu, dd) { max(abs(dd-mu)) }

## Passo 2 : obter o conjunto de observacoes ----
set.seed(123)
y <- rpois(100, lambda = 3)

## Passo 3 : otimizando a função perda ----

# Perda quadrática
fit_quad <- optimize(f = perda_quad, interval = c(0, 20), dd = y)
fit_quad
# Perda absoluta
fit_abs <- optimize(f = perda_abs, interval = c(0, 20), dd = y)
fit_abs
# Perda minimax
fit_minimax <- optimize(f = perda_minimax, interval = c(0, 20), dd = y)
fit_minimax








