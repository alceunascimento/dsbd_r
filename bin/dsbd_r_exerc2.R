# setup
library(dplyr)


# Da apostila ----
# Notas
notas <- c("João" = 7.3,
          "Maria" = 5.1,
          "Pedro" = 8.9,
          "Ana" = 6.5)
# ifelse
ifelse(notas >= 7, "aprovado", ifelse(notas >=4, "recuperação", "reprovado"))


# Tidyver : usando o case_when
dplyr::case_when(notas >= 7 ~ "Aprovado",
                 notas >= 4 ~ "Recuperação",
                 TRUE ~ "Reprovado")


# Exercicio 2 ----

## 1. Para quaisquer ponto (x,y) e um plano cartesiano, indique a qual quadrante esse ponto pertence.

# Definindo as coordenadas do ponto
x <- 2
y <- -3

# Determinar o quadrante
if (x > 0 & y > 0) {
  cat("O ponto (", x, ",", y, ") pertence ao primeiro quadrante.\n", sep = "")
} else if (x < 0 & y > 0) {
  cat("O ponto (", x, ",", y, ") pertence ao segundo quadrante.\n", sep = "")
} else if (x < 0 & y < 0) {
  cat("O ponto (", x, ",", y, ") pertence ao terceiro quadrante.\n", sep = "")
} else if (x > 0 & y < 0) {
  cat("O ponto (", x, ",", y, ") pertence ao quarto quadrante.\n", sep = "")
} else {
  cat("O ponto (", x, ",", y, ") está no eixo x ou y.\n", sep = "")
}



## 2. Crie um código que, dado um número, indique se ele é par ou ímpar

# Definindo o número
numero <- 7

# Determinar se o número é par ou ímpar (%% retorna o resto da divisao - op modulo)
if (numero %% 2 == 0) {
  cat(numero, "é um número par.\n")
} else {
  cat(numero, "é um número ímpar.\n")
}


##

# Número de lançamentos
n <- 1000
soma <- numeric(n)
soma
for (i in 1:n) {
  # Lançamento dos dados
  dado1 <- sample(1:6, 1, replace = FALSE)
  dado2 <- sample(1:6, 1, replace = FALSE)
  # Soma dos valores
  soma[i] <- dado1 + dado2
}
table(soma)


# Exercicio 4
  
"""
Utilizado o "for" loop, calcule o primeiro número da sequência de Fibonacci. 
A sequência de Fibonacci começa com 1, e os números subsequentes são a soma dos dosi anteriores.
(1,1,2,3,5,8,...)
"""

# Definindo as duas primeiras posições da sequência de Fibonacci
fibonacci <- c(1, 1)
# Iterando para encontrar o primeiro número não-zero
for (i in 3:100) {
  fibonacci[i] <- fibonacci[i - 1] + fibonacci[i - 2]
  if (fibonacci[i] != 0) {
    cat("O primeiro número na sequência de Fibonacci é:", fibonacci[i], "\n")
    break
  }
}


n <- 10
fibo <- numeric(n)
fibo[1] <- 1
fibo[2] <- 1
fibo
for (i in 3:n){
  fibo[i] <- fibo[i - 1] + fibo[i - 2]
}
fibo
fibo[3]



# While
i <- 1
i
while(i <= 5){
  print(i)
  i <- i + 1
}
i




soma <- 0
n <-0
while(soma >= 5) {
  dado1 <- sample(1:6, 1, replace = FALSE)
  dado2 <- sample(1:6, 1, replace = FALSE)
  soma <- dado1 + dado2
  n <- n + 1
}
n
