# UFPR - DSBD ----
# Estastistica - Prof. Wagner
# Exercicio 03 - Metodos Numericos ----

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
Encontre os valores de β0 e β1 que otimizam a seguinte função 
(descobrir se é um problema de maximização ou minimização é parte da questão). 
Utilize o método Gradiente Conjugado (CG) usando as opções default da função optim() em R.

L= − 1 / n \Sum^n_{i=1} = (y_i ln(μ_i) + (1 − y_i) ln(1 − μ_i) ), onde μ_i = exp(β_0 + β_1 x_i) 1 + exp(β_0 + β_1 x_i).

Como valores para yi  e xi considere os seguintes valores. 
Na notação n é o número de observações. 
Neste exercício n=10. 
Sua resposta são três valores: valor de β0 , valor de β1  e valor da função objetivo no ponto ótimo. 
Use três casas decimais. 
Use como valores iniciais β0=0 e β1=0.
xi
##  [1] 2 1 3 0 3 3 4 2 1 1
yi
##  [1] 1 0 0 0 1 0 1 0 0 0
"""


# Dados fornecidos
xi <- c(2, 1, 3, 0, 3, 3, 4, 2, 1, 1)
yi <- c(1, 0, 0, 0, 1, 0, 1, 0, 0, 0)
n <- length(xi)

# Função objetivo
objective_function <- function(beta) {
  beta0 <- beta[1]
  beta1 <- beta[2]
  mu <- exp(beta0 + beta1 * xi) / (1 + exp(beta0 + beta1 * xi))
  L <- - 1*(yi * log(mu) + (1 - yi) * log(1 - mu)) / n
  return(L)
}

# Valores iniciais
initial_values <- c(0, 0)

# Otimização utilizando o método Gradiente Conjugado (CG)
result <- optim(par = initial_values, fn = objective_function)

# Extraindo os resultados
beta0 <- result$par[1]
beta1 <- result$par[2]
optimal_value <- result$value

# Exibindo os resultados com três casas decimais
cat("Valor de beta0:", round(beta0, 3), "\n")
cat("Valor de beta1:", round(beta1, 3), "\n")
cat("Valor da função objetivo no ponto ótimo:", round(optimal_value, 3), "\n")


# 2 ----
rm(list = ls())

"""
Encontre os valores de β0 e β1 que otimizam a seguinte função
(descobrir se é um problema de maximização ou minimização é parte da questão). 
Utilize o método BFGS usando as opções default da função optim() em R.

L = \Sum^n_{i=1} (yi−μi)^2, onde μ_i = β_0 + β_1 x_i.

Como valores para yi e xi considere os seguintes valores. 
Na notação n é o número de observações. 
Neste exercício n=10. 
Sua resposta são três valores: valor de β0 , valor de β1  e valor da função objetivo no ponto ótimo. 
Use três casas decimais. 
Use como valores iniciais β0=0 e β1=0.
xi
##  [1] 4 5 4 2 2 0 1 4 1 2
yi
##  [1]  1.069  1.597  1.290 -0.747 -0.847  0.197 -1.049  1.303 -0.557 -0.958
"""
# Dados fornecidos
xi <- c(4, 5, 4, 2, 2, 0, 1, 4, 1, 2)
yi <- c(1.069, 1.597, 1.290, -0.747, -0.847, 0.197, -1.049, 1.303, -0.557, -0.958)
n <- length(xi)

# Função objetivo
objective_function <- function(beta) {
  beta0 <- beta[1]
  beta1 <- beta[2]
  mu <- beta0 + beta1 * xi
  L <- sum((yi - mu)^2)
  return(L)
}

# Valores iniciais
initial_values <- c(0, 0)

# Otimização utilizando o método BFGS
result <- optim(par = initial_values, fn = objective_function, method = "BFGS")

# Extraindo os resultados
beta0 <- result$par[1]
beta1 <- result$par[2]
optimal_value <- result$value

# Exibindo os resultados com três casas decimais
cat("Valor de beta0:", round(beta0, 3), "\n")
cat("Valor de beta1:", round(beta1, 3), "\n")
cat("Valor da função objetivo no ponto ótimo:", round(optimal_value, 3), "\n")





# 3 ----
rm(list = ls())

"""
Obtenha a derivada numérica usando o método de diferença finita central com dois pontos da função

f(x)= 7x^2 + 6x +   2
e avalie no ponto x=5.
A sua resposta é um número. 
Use três casas decimais se necessário. 
Use h=0.001.
"""
# Definindo a função f(x)
f <- function(x) {
  return(7*(x^2) + 6*x + 2)
}

# Ponto onde queremos avaliar a derivada
x <- 5

# Tamanho do intervalo (passo)
h <- 0.001

# Calculando a derivada numérica usando o método de diferença finita central
df_numeric <- ( f(x + h) - f(x - h)) / (2 * h)

# Exibindo o resultado
cat("A derivada numérica de f(x) no ponto x = 5 é:", df_numeric, "\n")



# 4 ----
rm(list = ls())
"""
Resolva a seguinte integral usando algum método da classe dos métodos baseados em quadratura Gaussiana. 
A escolha do método adequado é parte da questão. 
Use 21 pontos de integração. 
Use λ igual a 7867. 
Sua resposta é um número. 
Use três casas decimais.

\int_0^{\infty} (\lambda 3^{-t}) exp(-0.12t)dt
"""

library(pracma)

# Função Gauss-Laguerre (gaussLaguerre do pkcg pracma)
gauss_laguerre <- function(integrando, n.pontos, ...){
  pontos <- gaussLaguerre(n.pontos)
  integral <- sum(pontos$w * integrando(pontos$x, ...) / exp(-pontos$x))
  return(integral)
}
# função a ser integrada
f <- function(t, lambda) {
  return((lambda * 3^(-t)) * exp(-0.12 * t))
}

n <- 21
lambda <- 7867

# Calculando a integral
integral <- gauss_laguerre(integrando = f, n.pontos = n, lambda = lambda)
integral




# 5 ----
rm(list = ls())
"""
Resolva a seguinte integral usando o método de Gauss-Legendre. 
Use 19 pontos de integração e considere o limite inferior de integração (a=0.1) 
e o limite superior de integração (b = 12.1). 
Na Equação considere os valores de λ igual a 8 e k igual a 5. 
Sua resposta é um número. Use três casas decimais.
 
 \int_{a}^b \frac{k}{\lambda} + \left( \frac{y}{\lambda} \right)^{k-1} exp^{-(\frac{y}{\lambda})^k} dy

"""

# Carregando o pacote pracma
library(pracma)

# Função Gauss-Laguerre
gauss_laguerre <- function(integrando, n.pontos, ...){
  pontos <- gaussLaguerre(n.pontos)
  integral <- sum(pontos$w * integrando(pontos$x, ...) / exp(-pontos$x))
  return(integral)
}

# Definição da função a ser integrada
integrando <- function(y, alpha, beta) {
  return((beta^alpha * y^(alpha-1) * exp(-beta * y)) / gamma(alpha))
}

# Parâmetros fornecidos
a <- 0.1
b <- 8.1
alpha <- 2
beta <- 3
n <- 15  # Número de pontos de integração

# Calculando a integral usando a Quadratura de Gauss-Laguerre
integral <- gauss_laguerre(integrando, n, alpha = alpha, beta = beta)

# Exibindo o resultado
cat("O valor da integral é:", round(integral, 3), "\n")


# 6 ----
rm(list = ls())
"""
Resolva a seguinte integral usando o método de Gauss-Legendre. 
Use 15 pontos de integração e considere o limite inferior de integração 
(a= 0.1) 
e o limite superior de integração (b= 8.1). 
Na Equação considere os valores de α igual a 2 e β igual a 3. 
Sua resposta é um número. Use três casas decimais.

\int_{a}^b \frac{\beta^\alpha y^{\alpha-1} exp^{-\beta y}}{\Gamma(\alpha)} dy


"""




# 7 ----
rm(list = ls())
"""
Com relação as abordagens para diferenciação numérica assinale a única alternativa incorreta.
Escolha uma opção:
a. Se baseiam na ideia de aproximar o limite que define a derivada de forma aproximada computacionalmente.
b. São métodos baseados em diferenças finitas.
c. São métodos baseados em aproximar a função original por uma outra função de fácil derivação.*
d. O estado da arte em termos de derivadas numéricas é o método de diferenciação automática.
e. São métodos baseados em truncar uma soma infinita.
"""




# 8 ----
rm(list = ls())
"""
Resolva a seguinte integral usando o método de Gauss-Legendre. 
Use 18 pontos de integração e considere o limite inferior de integração 
(a= 0.1) e o limite superior de integração (b= 8.1). 
Na Equação considere os valores de λ igual a 12 e k igual a 5. 
Sua resposta é um número. Use três casas decimais.

\int_{a}^b \frac{\beta^\alpha y^{\alpha-1} exp^{-\beta y}}{\Gamma(\alpha)} dy


"""
# Função Gauss-Laguerre
gauss_laguerre <- function(integrando, n.pontos, ...){
  pontos <- gaussLaguerre(n.pontos)
  integral <- sum(pontos$w * integrando(pontos$x, ...) / exp(-pontos$x))
  return(integral)
}

# Definição da função a ser integrada
integrando <- function(y, lambda, k) {
  return((k/lambda) * (y/lambda)^(k-1) * exp(-(y/lambda)^k))
}

# Parâmetros fornecidos
a <- 0.1
b <- 8.1
lambda <- 12
k <- 5
n <- 18  # Número de pontos de integração

# Calculando a integral usando a Quadratura de Gauss-Laguerre
integral <- gauss_laguerre(integrando, n, lambda = lambda, k = k)

# Exibindo o resultado
cat("O valor da integral é:", round(integral, 3), "\n")






# 9 ----
rm(list = ls())
"""
Calcule as derivadas parciais numéricas de primeira e segunda ordem da seguinte função de duas variáveis: 
u = exp^x^2 + exp^y^2
Use o método de Richardson com h = 0.00001. 
Avalie cada uma das derivadas no ponto x=1  e y=2 . 
A resposta são seis valores numéricos na seguinte ordem: 
u_x; u_y; u_{xx}; u_{yy}; u_{xy} e u_{yx}. 
Use arredondamento com três casas decimais para a sua resposta.
"""

# Função a ser diferenciada
f <- function(x, y) {
  return(exp(x^2) + exp(y^2))
}

# Ponto de avaliação
x <- 1
y <- 2

# Passo
h <- 0.00001

# Derivadas parciais de primeira ordem (ux e uy) usando o método de Richardson
ux <- (f(x + h, y) - f(x, y)) / h
uy <- (f(x, y + h) - f(x, y)) / h

# Derivadas parciais de segunda ordem (uxx, uyy e uxy) usando o método de Richardson
uxx <- (f(x + 2*h, y) - 2*f(x + h, y) + f(x, y)) / (h^2)
uyy <- (f(x, y + 2*h) - 2*f(x, y + h) + f(x, y)) / (h^2)
uxy <- (f(x + h, y + h) - f(x + h, y) - f(x, y + h) + f(x, y)) / (h^2)

# Exibindo os resultados
cat("u_x:", round(ux, 3), "\n")
cat("u_y:", round(uy, 3), "\n")
cat("u_xx:", round(uxx, 3), "\n")
cat("u_yy:", round(uyy, 3), "\n")
cat("u_xy:", round(uxy, 3), "\n")
cat("u_yx:", round(uxy, 3), "\n")





# 10 ----
rm(list = ls())
"""
Determine a raiz cúbica de 300 obtendo a solução numérica da equação 
f(x)=x^3−300=0
usando o método da regula falsi. 
Use tolerância de 0.0001. 
A escolha do intervalo adequado é parte da questão. 
Como critério para parada utilize o erro estimado, ou seja, 
|x_{i+1}−x_{i}|<ϵ
, onde ϵ é o erro tolerado. 
A sua resposta é o valor da raiz use três casas decimais se necessário.
"""

# Função que define a equação
f <- function(x) {
  return(x^3 - 300)
}

# Intervalo inicial
a <- 6
b <- 7

# Erro tolerado
epsilon <- 0.0001

# Critério de parada
erro <- Inf

# Loop para encontrar a raiz
while (erro > epsilon) {
  # Calculando os valores de f(a) e f(b)
  fa <- f(a)
  fb <- f(b)
  
  # Calculando o próximo ponto usando o método da Regula Falsi
  x <- (a * fb - b * fa) / (fb - fa)
  
  # Calculando o valor de f(x)
  fx <- f(x)
  
  # Atualizando o intervalo
  if (fx * fa < 0) {
    b <- x
  } else {
    a <- x
  }
  
  # Atualizando o erro
  erro <- abs(fx)
}

# Exibindo o valor da raiz
cat("A raiz cúbica de 300 é:", round(x, 3), "\n")



# 11 ----
rm(list = ls())
"""
Resolva a seguinte integral usando algum método da classe dos métodos baseados em quadratura Gaussiana. A escolha do método adequado é parte da questão. Use 19 pontos de integração. Na equação o valor de λ
 é igual a -0.033. Sua resposta é um número. Use três casas decimais.

\int_{-\infty}^{\infty} \lambda \; exp(-(x-\lambda)^2)dx
"""

# Função gauss_laguerre adaptada para calcular a integral fornecida
gauss_laguerre <- function(integrando, n.pontos, lambda) {
  pontos <- gaussLaguerre(n.pontos)
  integral <- sum(pontos$w * integrando(pontos$x, lambda))
  return(integral)
}

# Função a ser integrada
integrando <- function(x, lambda) {
  return(lambda * exp(-x^2))
}

# Parâmetros fornecidos
lambda <- -0.033
n <- 19  # Número de pontos de integração

# Calculando a integral usando a função gauss_laguerre
integral <- gauss_laguerre(integrando, n, lambda)

# Exibindo o resultado
cat("O valor da integral é:", round(integral, 3), "\n")




# 12 ----
rm(list = ls())
"""
Resolva a seguinte integral usando o método de Gauss-Legendre. 
Use 22 pontos de integração e considere o limite inferior de integração 
(a=0.1) e o limite superior de integração (b= 11.1). 
Na Equação considere os valores de α igual a 2 e β  igual a 3. 
Sua resposta é um número. Use três casas decimais.

\int_{a}^b \frac{\beta^\alpha y^{\alpha-1} exp^{-\beta y}}{\Gamma(\alpha)} dy
"""


# Carregando o pacote pracma
library(pracma)

# Função Gauss-Laguerre
gauss_laguerre <- function(integrando, n.pontos, ...){
  pontos <- gaussLaguerre(n.pontos)
  integral <- sum(pontos$w * integrando(pontos$x, ...) / exp(-pontos$x))
  return(integral)
}

# Definição da função a ser integrada
integrando <- function(y, alpha, beta) {
  return((beta^alpha * y^(alpha-1) * exp(-beta * y)) / gamma(alpha))
}

# Parâmetros fornecidos
a <- 0.1
b <- 8.1
alpha <- 2
beta <- 3
n <- 22  # Número de pontos de integração

# Calculando a integral usando a Quadratura de Gauss-Laguerre
integral <- gauss_laguerre(integrando, n, alpha = alpha, beta = beta)

# Exibindo o resultado
cat("O valor da integral é:", round(integral, 3), "\n")




# 13 ----
rm(list = ls())
"""
Resolva a seguinte integral usando algum método da classe dos métodos baseados em quadratura Gaussiana. 
A escolha do método adequado é parte da questão. 
Use 17 pontos de integração. 
Na equação o valor de λ é igual a 9. 
Sua resposta é um número. Use três casas decimais.

\int_{-\infty}^{\infty} \lambda \; \frac{exp^{-\sqrt{x}}}{\sqrt{x}} dx 
"""
# Função gauss_laguerre adaptada para calcular a integral fornecida
gauss_laguerre <- function(integrando, n.pontos, lambda) {
  pontos <- gaussLaguerre(n.pontos)
  integral <- sum(pontos$w * integrando(pontos$x, lambda))
  return(integral)
}

# Função a ser integrada
integrando <- function(x, lambda) {
  return(lambda * exp(-sqrt(x)) / sqrt(x))
}

# Parâmetros fornecidos
lambda <- 9
n <- 17  # Número de pontos de integração

# Calculando a integral usando a função gauss_laguerre
integral <- gauss_laguerre(integrando, n, lambda)

# Exibindo o resultado
cat("O valor da integral é:", round(integral, 3), "\n")




# 14 ----
rm(list = ls())
"""
Resolva a seguinte integral usando algum método da classe dos métodos baseados em quadratura Gaussiana. 
A escolha do método adequado é parte da questão. Use 21 pontos de integração. 
Use λ igual a 8098. 
Sua resposta é um número. Use três casas decimais.

\int_0^{\infty} (\lambda 3^{-t}) exp(-0.12t)dt
"""

# Função gauss_laguerre adaptada para calcular a integral fornecida
gauss_laguerre <- function(integrando, n.pontos, lambda) {
  pontos <- gaussLaguerre(n.pontos)
  integral <- sum(pontos$w * integrando(pontos$x, lambda))
  return(integral)
}

# Função a ser integrada
integrando <- function(t, lambda) {
  return(lambda * 3^(-t) * exp(-0.12 * t))
}

# Parâmetros fornecidos
lambda <- 8098
n <- 21  # Número de pontos de integração

# Calculando a integral usando a função gauss_laguerre
integral <- gauss_laguerre(integrando, n, lambda)

# Exibindo o resultado
cat("O valor da integral é:", round(integral, 3), "\n")




# 15 ----
rm(list = ls())
"""
Resolva a seguinte integral usando algum método da classe dos métodos baseados em simulação. 
A escolha do método adequado é parte da questão. 
Use 975 pontos de integração. 
Na equação o valor de κ  é igual a 2. 
Como este método é baseado em simulação antes de usá-lo fixe a semente em 123 usando o comando set.seed(123).

\int_{-\infty}^{\infty} \frac{1}{k + k^{-1}} exp(\frac{-x}{k})dx

"""

# Fixando a semente para garantir reprodutibilidade
set.seed(123)

# Número de pontos de integração
n <- 975

# Valor de κ
k <- 2

# Função a ser integrada
integrando <- function(x, k) {
  return(1 / (k + 1/k) * exp(-x / k))
}

# Amostragem de Monte Carlo
amostras <- rexp(n, rate = k)

# Calculando a média das amostras multiplicadas pela função integrando
integral <- mean(integrando(amostras, k))

# Exibindo o resultado
cat("O valor da integral é:", round(integral, 3), "\n")



# 16 ----
rm(list = ls())
"""
Com relação aos métodos de diferenciação numérica, assinale a única alternativa correta.
Escolha uma opção:
a. Podem ser divididos entre métodos de confinamento e abertos.
b. São indicados para quando a função que se quer derivar é cara de se obter computacionalmente.
c. Não são indicados no caso da função a ser derivada não seja totalmente conhecida.
d. Alguns se baseiam na ideia de truncar uma soma infinita.*
e. Alguns se baseiam na ideia de aproximar a função original por uma outra função de fácil derivação.
"""




# 17 ----
rm(list = ls())
"""
Sobre o Método Trapezoidal, assinale a alternativa correta.
Escolha uma opção:
a. O método usa uma função linear para aproximar o integrando e integra a função aproximada de forma usual.*
b. Neste método o integrando é aproximado usando um polinômio de segundo grau.
c. A ideia básica é reescrever o integrando como um somatório.
d. O método corresponde a usar o método Gauss-Hermite adaptativo com apenas um ponto de integração.
e. A ideia é tratar a integral como uma quantidade desconhecida da qual podemos retirar amostras para então estimar o valor da integral como uma média simples.
"""




# 18 ----
rm(list = ls())
"""
Resolva a seguinte integral usando o método de Gauss-Legendre. 
Use 20 pontos de integração e considere 
o limite inferior de integração (a=0.1) e 
o limite superior de integração (b= 13.1). 
Na Equação considere os valores de α igual a 2 e β igual a 2. 
Sua resposta é um número. Use três casas decimais.
 
 \int_{a}^b \frac{\beta^\alpha y^{\alpha-1} exp^{-\beta y}}{\Gamma(\alpha)}dy
"""
# Função gauss_laguerre adaptada para calcular a integral fornecida
gauss_laguerre <- function(integrando, n.pontos, lambda) {
  pontos <- gaussLaguerre(n.pontos)
  integral <- sum(pontos$w * integrando(pontos$x, lambda))
  return(integral)
}

# Função a ser integrada
integrando <- function(y, alpha, beta) {
  return((beta^alpha * y^(alpha - 1) * exp(-beta * y)) / gamma(alpha))
}

# Parâmetros fornecidos
a <- 0.1
b <- 13.1
alpha <- 2
beta <- 2
n <- 20  # Número de pontos de integração
lambda <- 1  # Valor de lambda não é utilizado na função gauss_laguerre

# Função a ser integrada
integrando <- function(y, alpha, beta) {
  return((beta^alpha * y^(alpha - 1) * exp(-beta * y)) / gamma(alpha))
}

# Calculando a integral usando a função gauss_laguerre
resultado <- gauss_laguerre(integrando, n, beta)

# Exibindo o resultado
cat("O valor da integral é:", round(resultado, 3), "\n")





# 19 ----
rm(list = ls())
"""

"""




# 20 ----
rm(list = ls())
"""

"""




# 21 ----
rm(list = ls())
"""

"""




# 22 ----
rm(list = ls())
"""

"""




# 23 ----
rm(list = ls())
"""

"""




# 24 ----
rm(list = ls())
"""

"""




# 25 ----
rm(list = ls())
"""

"""




# 26 ----
rm(list = ls())
"""

"""




# 27 ----
rm(list = ls())
"""

"""




# 28 ----
rm(list = ls())
"""

"""




# 29 ----
rm(list = ls())
"""

"""




# 30 ----
rm(list = ls())
"""

"""




# 31 ----
rm(list = ls())
"""

"""




# 32 ----
rm(list = ls())
"""

"""





