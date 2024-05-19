# UFPR - DSBD ----
# Estastistica - Prof. Wagner
# Exercicio 02 - Algebra linear ----

# setup ----
library(expm)
library(matlib)
library(Rlinsolve)
library(stats)
library(car)
rm(list = ls())

# 1 ----
rm(list = ls())
"""
Obtenha a exponencial matricial da matriz A abaixo. 
Para inserir a sua resposta calcule o traço da matriz resultante.

A
     1   2   3   4
 1 1.0 0.8 0.8 0.7
 2 0.8 1.0 0.7 0.8
 3 0.8 0.7 1.0 0.8
 4 0.7 0.8 0.8 1.0
"""

# Criando a matriz
A <- matrix(c(1.0, 0.8, 0.8, 0.7,
                   0.8, 1.0, 0.7, 0.8,
                   0.8, 0.7, 1.0, 0.8,
                   0.7, 0.8, 0.8, 1.0), 
                 nrow = 4, 
                 ncol = 4, 
                 byrow = TRUE)
A
# Calculando a exponencial matricial de A
expmA <- expm(A)
expmA
# Calculando o traço da matriz resultante
sum(diag(expmA))


# 2 ----
rm(list = ls())
"""
# Considere o vetores a e b
a <-c(8, 11, 15,  5, 10)
b <- c(9,  8, 12, 15,  9)
Calcule o produto de Hadamard de a por b, ou seja c=a⨀b
Para incluir sua resposta faça a soma de c
A resposta deve ser um único valor numérico com até três casas decimais (se necessário).
"""
a <-c(8, 11, 15,  5, 10)
b <- c(9,  8, 12, 15,  9)
# Produto de Hadamard de a por b
c <- a * b
# Soma dos elementos de c
sum(c)



# 3 ----
rm(list = ls())
"""
No contexto de métodos iterativos para solução de sistemas lineares. 
Em qual situação o método para?
Escolha uma opção:
  
a. Quando algum critério de parada é atingido.*
b. Quando a solução exata é obtida.
c. Quando o sistema apresenta pivôs grandes o suficiente para aplicação do método de Gauss.
d. Quando o sistema apresenta pivôs grandes o suficiente para aplicação do método de Gauss-Jordan.
e. O método para automaticamente após 10 iterações.
"""



# 4 -----
rm(list = ls())
"""  
# Considere o vetor a e o escalar alpha:

# Vetor a
a <- c(8, 7, 8, 12, 4)

# Escalar alpha
alpha <- 6

Calcule a multiplicação de b=αa
Para incluir sua resposta faça a soma de b
A resposta deve ser um único valor numérico com até três casas decimais (se necessário).
"""
a <- c(8, 7, 8, 12, 4)
alpha <- 6
b <- alpha * a
sum(b)


# 5 -----
rm(list = ls())
"""
Qual é a condição que permite que duas matrizes possam ser somadas ou subtraídas?
Escolha uma opção:

a. As matrizes precisam ser quadradas.
b. As matrizes precisam ter o mesmo número de linhas.
c. As matrizes precisam ter o mesmo número de colunas.
d. As matrizes precisam ter o mesmo número de linhas e colunas.*
e. As matrizes precisam ser idênticas.
"""


# 6 -----
rm(list = ls())
"""
# Considere o vetor a e o escalar alpha:
  
a <- c(10, 11,  7, 13, 18)
alpha <- 10

Calcule a multiplicação de b=αa
. Para incluir sua resposta faça a soma de b
. A resposta deve ser um único valor numérico com até três casas decimais (se necessário).
"""
a <- c(10, 11,  7, 13, 18)
alpha <- 10
b <- alpha * a
sum(b)



# 7 ----
rm(list = ls())
"""
Use o método de Gauss-Seidel para resolver o sistema Ax=b onde A e b são dados abaixo. 
Quantas iterações foram necessárias para o método convergir? 
Use como valores iniciais um vetor de zeros. 
Sua resposta deve ser apenas um número inteiro.

A
##      [,1] [,2] [,3] [,4]
## [1,]  2.5  0.8  0.8  0.7
## [2,]  0.8  2.5  0.7  0.8
## [3,]  0.8  0.7  2.5  0.8
## [4,]  0.7  0.8  0.8  2.5
b
## [1] 10  9  8 17
"""

# Matriz A e vetor b
A <- matrix(c(2.5, 0.8, 0.8, 0.7,
              0.8, 2.5, 0.7, 0.8,
              0.8, 0.7, 2.5, 0.8,
              0.7, 0.8, 0.8, 2.5), 
            nrow = 4, 
            ncol = 4, 
            byrow = TRUE)
b <- c(10, 9, 8, 17)
sol <- lsolve.gs(A, b, xinit = c(0,0,0,0), reltol = 1e-04)
sol$iter

ERREI



# 8 ----
rm(list = ls())
"""
Considere o vetores a e b :
a
## [1]  7 12  7 11  9
b
## [1] 13  9 10 12  7

Calcule o produto de Hadamard de a por b , ou seja c=a⨀b
Para incluir sua resposta faça a soma de c
A resposta deve ser um único valor numérico com até três casas decimais (se necessário).
"""
a <- c(7, 12, 7, 11, 9)
b <- c(13, 9, 10, 12, 7)
c <- a * b
sum(c)


# 9 ----
rm(list = ls())
"""
Obtenha os autovalores da matriz A. 
Sua resposta deve ser quatro valores. 
Use três casas decimais (se necessário).

A
##   1 2 3 4
## 1 0 0 0 0
## 2 0 0 0 0
## 3 0 0 0 0
## 4 0 0 0 0
"""

# matriz nula, eigenvalues são zero.

# 10 -----
rm(list = ls())
"""
Considere a decomposição em autovalores e autovetores de uma matriz A como apresentado abaixo. 
Obtenha o determinante de A.

eigen(A)
## eigen() decomposition
## $values
## [1] 19.8  1.8  1.8  0.6
## 
## $vectors
##      [,1]          [,2]          [,3] [,4]
## [1,] -0.5  0.000000e+00  7.071068e-01  0.5
## [2,] -0.5 -7.071068e-01 -6.106227e-16 -0.5
## [3,] -0.5  7.071068e-01  3.885781e-16 -0.5
## [4,] -0.5  1.054712e-15 -7.071068e-01  0.5
"""

# Vetor de autovalores
autovalores <- c(19.8, 1.8, 1.8, 0.6)
autovalores
# Matriz de autovetores
autovetores <- matrix(c(-0.5, -0.5, -0.5, -0.5,
                        0, -0.7071068, 0.7071068, 1.054712e-15,
                        0.7071068, -6.106227e-16, 3.885781e-16, -7.071068e-01,
                        0.5, -0.5, -0.5, 0.5), 
                      nrow = 4, 
                      ncol = 4, 
                      byrow = FALSE)
autovetores
# Reconstruir a matriz A
A <- autovetores %*% diag(autovalores) %*% solve(autovetores)
# Calcular o determinante de A
det(A)



# 11 ----
rm(list = ls())
"""
Considere o vetores a e b:
a
## [1] 7 7 6 9 9
b
## [1]  8  6 19 11  9

Calcule o produto de Hadamard de a por b, ou seja c=a⨀b
Para incluir sua resposta faça a soma de c
A resposta deve ser um único valor numérico com até três casas decimais (se necessário).
"""
a <- c(7, 7, 6, 9, 9)
b <- c(8, 6, 19, 11, 9)
c <- a * b
sum(c)


# 12 ----
"""
Qual é a condição para que o produto entre matrizes seja possível?
Escolha uma opção:
a. As matrizes precisam ser quadradas.
b. O número de colunas da primeira matriz deve ser igual ao número de colunas da segunda matriz.
c. O número de linhas da primeira matriz deve ser igual ao número de linhas da segunda matriz.
d. O número de linhas da primeira matriz deve ser igual ao número de colunas da segunda matriz.
e. O número de colunas da primeira matriz deve ser igual ao número de linhas da segunda matriz.*
"""

# 13 ----
"""
Como é a estrutura de uma matriz Identidade?
Escolha uma opção:
a. Os elementos diagonais são diferentes de zero.
b. Uma matriz diagonal onde os elementos diagonais são todos iguais a 1*
c. Os elementos abaixo da diagonal são todos iguais a zero.
d. Os elementos acima da diagonal são todos iguais a zero.
e. Todas as entradas são iguais a zero.
"""


# 14 ----
# repete a 10



# 15 ----
rm(list = ls())
"""
Considere o escalar α e a matriz A conforme abaixo:
A
##      [,1] [,2] [,3] [,4] [,5]
## [1,]   12   16    7    7    6
## [2,]    7   12   15    5    9
## [3,]    8    8    7   12    5
## [4,]   13   10   12   10    5
## [5,]   14    7   11   11    9
alpha
## [1] 3

Calcule a matriz C=αA
Para incluir a sua resposta some todos os elementos da matriz C
Sua resposta deve ser um único número. Use três casas decimais (se necessário).
"""
A <- matrix(c(12, 16, 7, 7, 6,
              7, 12, 15, 5, 9,
              8, 8, 7, 12, 5,
              13, 10, 12, 10, 5,
              14, 7, 11, 11, 9), 
            nrow = 5, 
            byrow = TRUE)
A
alpha <- 3
C <- alpha * A
sum(C)



# 16 ----
rm(list = ls())
"""
A decomposição em autovalores e autovetores pode ser usada para obter que tipo especial de matriz?
Escolha uma opção:
a. Identidade.
b. Triangular.
c. Transposta.
d. Diagonal.*
e. Inversa.
"""
ERREI


# 17 ----
rm(list = ls())
"""
Considere matrizes A, B e C compatíveis e α  e β  escalares. 
As seguintes propriedades são verdadeiras, exceto:
Escolha uma opção:
a. A+B = B+A
b. α(A+B)=(αB+αA)
c. αA⋅B=B⋅(αA)
d. (α+β)A=αA+βB*
e. A(B−C)=AB−AC
"""
ERREI

# 18 ----
rm(list = ls())
"""
Obtenha os autovalores da matriz A.
Sua resposta deve ser quatro valores. 
Use três casas decimais (se necessário).

A
##     1   2   3   4
## 1 2.0 1.6 1.6 1.6
## 2 1.6 2.0 1.6 1.6
## 3 1.6 1.6 2.0 1.6
## 4 1.6 1.6 1.6 2.0

"""
A <- matrix(c(2.0, 1.6, 1.6, 1.6,
              1.6, 2.0, 1.6, 1.6,
              1.6, 1.6, 2.0, 1.6,
              1.6, 1.6, 1.6, 2.0), 
            nrow = 4, 
            byrow = TRUE)
A
eigen(A)$values




# 19 ----
rm(list = ls())
"""
Em que consiste a decomposição LU?
Escolha uma opção:
a. Decompor ou fatorar uma matriz em um produto de duas matrizes onde uma é triangular inferior e a outra é triangular superior.*
b. Decompor ou fatorar uma matriz em um produto de duas matrizes onde uma é triangular inferior e a outra é diagonal.
c. Decompor ou fatorar uma matriz em um produto de duas matrizes diagonais.
d. Decompor ou fatorar uma matriz em um produto de duas matrizes onde uma é uma identidade e a outra é triangular superior.
e. Decompor ou fatorar uma matriz em um produto de duas matrizes onde uma é triangular inferior e a outra é uma identidade.
"""

# 20 ----
rm(list = ls())
"""
Quais formatos tornam o sistema de fácil solução?
Escolha uma opção:
a. Singular, quadrado e simétrico.
b. Triangular superior, triangular inferior e diagonal.*
c. Singular, triangular inferior e diagonal.
d. Triangular superior, triangular inferior e simétrico.
e. Triangular superior, quadrado e diagonal.
"""
# metodos diretos se aplicam

# 21 ----
rm(list = ls())
"""
O que caracteriza uma matriz quadrada?
Escolha uma opção:
a. O mesmo número de linhas e colunas.*
b. Os elementos diagonais são diferentes de zero.
c. Os elementos abaixo da diagonal são todos iguais a zero.
d. Os elementos acima da diagonal são todos iguais a zero.
e. Todas as entradas são iguais a zero.
"""

# 22 ----
rm(list = ls())
"""
Considere as matrizes A  e B  conforme abaixo:
A
##      [,1] [,2] [,3] [,4] [,5]
## [1,]   11    9    7    8   14
## [2,]    8   10    4   12   13
## [3,]   13   10   11   11    6
## [4,]   11    8    9   13    7
## [5,]   11   13   11    8    5
B
##      [,1] [,2] [,3] [,4] [,5]
## [1,]    4    8   10   14    8
## [2,]    9    5   18   11   15
## [3,]    8   10   10   10   13
## [4,]    5   16    7    5   11
## [5,]   15   14   16   14   17

Calcule a matriz C=A+B
Para incluir a sua resposta some todos os elementos da matriz C
Sua resposta deve ser um único número. Use três casas decimais (se necessário).
"""
A <- matrix(c(11, 9, 7, 8, 14,
              8, 10, 4, 12, 13,
              13, 10, 11, 11, 6,
              11, 8, 9, 13, 7,
              11, 13, 11, 8, 5), 
            nrow = 5, 
            byrow = TRUE)
A
B <- matrix(c(4, 8, 10, 14, 8,
              9, 5, 18, 11, 15,
              8, 10, 10, 10, 13,
              5, 16, 7, 5, 11,
              15, 14, 16, 14, 17), 
            nrow = 5, 
            byrow = TRUE)
B
C <- A + B
C
sum(C)



# 23 ----
rm(list = ls())
"""
Considere o métodos de Gauss-Jordan para resolver sistemas lineares do tipo Ax=b. 
Faça a sua implementação do método de Gauss-Jordan e use para resolver o sistema onde A e b são dadas abaixo. 
A sua resposta terá 4 posições com a solução na ordem x1, x2, x3  e x4. 
A tolerância para esta questão é de 0,1. 
Note que os valores entre colchetes não fazem parte da matriz A.
A
##      [,1] [,2] [,3] [,4]
## [1,]  2.0  0.8  0.8  0.7
## [2,]  0.8  2.0  0.7  0.8
## [3,]  0.8  0.7  2.0  0.8
## [4,]  0.7  0.8  0.8  2.0
b
## [1] 18 11 12  8
"""
A <- matrix(c(2.0, 0.8, 0.8, 0.7,
              0.8, 2.0, 0.7, 0.8,
              0.8, 0.7, 2.0, 0.8,
              0.7, 0.8, 0.8, 2.0), 
            nrow = 4, 
            byrow = TRUE)
b <- c(18, 11, 12, 8)
gaussianElimination(A, b, tol = 0.1, verbose=TRUE, fractions=FALSE)



# 24 ----
rm(list = ls())
"""
Qual é a principal diferença entre a decomposição em valores singulares e a decomposição em autovalores e autovetores?
Escolha uma opção:
a. A decomposição em valores singulares é definida para qualquer matriz, enquanto que a decomposição em autovalores e autovetores é definida apenas para matrizes simétricas.
b. A decomposição em autovalores e autovetores é definida para qualquer matriz, enquanto que a decomposição em valores singulares é definida apenas para matrizes simétricas.
c. A decomposição em valores singulares requer uma matriz que admita inversa, enquanto que a decomposição em autovalores e autovetores não.*
d. A decomposição em autovalores e autovetores requer uma matriz que admita inversa, enquanto que a decomposição em valores singulares não.
e. Não há qualquer diferença entre os métodos.
"""
ERREI

# 25 ----
rm(list = ls())
"""
Como é a estrutura de uma matriz triangular inferior?
Escolha uma opção:
a. Os elementos acima da diagonal são todos diferentes a zero.
b. Os elementos abaixo da diagonal são todos iguais a zero.
c. Os elementos diagonais são diferentes de zero.
d. Os elementos acima da diagonal são todos iguais a zero.*
e. Todas as entradas são iguais a zero.
"""

# 26 ----
rm(list = ls())
"""
Considere os vetores:
a
## [1]  7 11 13  7 11
b
## [1]  7  6  8 11 10

Calcule a subtração de a−b=c
Para incluir sua resposta faça a soma de c
A resposta deve ser um único valor numérico com até três casas decimais (se necessário).
"""
a <- c(7, 11, 13, 7, 11)
b <- c(7, 6, 8, 11, 10)
c <- a - b
c
sum(c)



# 27 ----
rm(list = ls())
"""
Considere os vetores:
a
## [1] 14 10  6 10  3
b
## [1]  9  6 12 13  9
Calcule a soma de a+b=c
Para incluir sua resposta faça a soma de c
A resposta deve ser um único valor numérico com até três casas decimais (se necessário).
"""
a <- c(14, 10, 6, 10, 3)
b <- c(9, 6, 12, 13, 9)
c <- a + b
c
sum(c)



# 28 ----
rm(list = ls())
"""
Considere a decomposição LU da matrix A dada abaixo. 
O traço da matriz U é? 
Sua resposta deve ser um número. 
Use três casas decimais (se necessário).

A
##     1   2   3   4
## 1 1.0 0.9 0.9 0.9
## 2 0.9 1.0 0.9 0.9
## 3 0.9 0.9 1.0 0.9
## 4 0.9 0.9 0.9 1.0
"""
A <- matrix(c(1.0, 0.9, 0.9, 0.9,
              0.9, 1.0, 0.9, 0.9,
              0.9, 0.9, 1.0, 0.9,
              0.9, 0.9, 0.9, 1.0),
            nrow = 4, 
            byrow = TRUE)
A

# Realizar a decomposição LU

## função LU (do prof. Wagner)
my_lu <- function(A) {
  n_row <- nrow(A)
  n_col <- ncol(A)
  SOL <- matrix(NA, n_row, n_col) ## Matriz para receber os resultados
  SOL[1,] <- A[1,]
  pivo <- matrix(0, n_col, n_row)
  for(j in 1:c(n_row-1)) {
    for(i in c(j+1):c(n_row)) {
      pivo[i,j] <- A[i,j]/SOL[j,j]
      SOL[i,] <- A[i,] - pivo[i,j]*SOL[j,]
      A[i,] <- SOL[i,]
    }
  }
  diag(pivo) <- 1
  return(list("L" = pivo, "U" = SOL)) }

## Decomposição
LU <- my_lu(A) 
LU

## Verificando a solução
LU$L %*% LU$U 

## Obtendo o traco de U em LU
LU$U
sum(diag(LU$U))
