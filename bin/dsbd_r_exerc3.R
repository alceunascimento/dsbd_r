
library(dplyr)

cv <- function(vetor){
  desvio  = sd(vetor, na.rm = T)
  media   = mean(vetor, na.rm = T)
  cv      = desvio / media * 100
  return(list(cv = cv, media = media, desvio = desvio))
}
cv_notas = cv(notas)
cv_notas


cv <- function(vetor){
  desvio  = sd(vetor, na.rm = T)
  media   = mean(vetor, na.rm = T)
  cv      = desvio / media * 100
  return(list(estats = data.frame(cv = cv, 
                                  media = media, 
                                  desvio = desvio)),
              dados = vetor
         )
}
cv_notas = cv(notas)
cv_notas



# Exercicios

## 1

classifica_combustivel <- function(meio_de_transporte) {
  if (meio_de_transporte == "Carro") {
    return("Gasolina, Diesel, Eletricidade ou Gás Natural")
  } else if (meio_de_transporte == "Moto") {
    return("Gasolina")
  } else if (meio_de_transporte == "Bicicleta") {
    return("Humana (sem combustível)")
  } else if (meio_de_transporte == "Ônibus") {
    return("Diesel ou Gás Natural")
  } else if (meio_de_transporte == "Trem") {
    return("Eletricidade ou Diesel")
  } else if (meio_de_transporte == "Avião") {
    return("Querosene")
  } else if (meio_de_transporte == "Barco") {
    return("Diesel ou Gasolina")
  } else {
    return("Meio de transporte não reconhecido")
  }
}

# Exemplo de uso da função
meio_de_transporte <- "Carro"
cat("O tipo de combustível utilizado pelo meio de transporte", meio_de_transporte, "é:", classifica_combustivel(meio_de_transporte), "\n")


## 2

converte_temperatura <- function(temperatura, unidade) {
  if (unidade == "celsius") {
    return(paste("A temperatura", temperatura, "graus Celsius é equivalente a", temperatura * 9/5 + 32, "graus Fahrenheit"))
  } else if (unidade != "celsius") {
    return(paste("A temperatura", temperatura, "graus Fahrenheit é equivalente a", (temperatura - 32) * 5/9, "graus Celsius"))
  } else {
    return("Unidade de temperatura não reconhecida")
  }
}

# Exemplo de uso da função
temperatura <- 20
unidade <- "celsius"
cat(converte_temperatura(temperatura, unidade), "\n")


## 3
calcula_imc <- function(peso, altura) {
  imc <- round(peso / (altura^2),3)
  if (imc < 18.5) {
    return(paste("IMC:", imc, "(abaixo do peso)"))
  } else if (imc >= 18.5 & imc <= 24.9) {
    return(paste("IMC:", imc, "(peso normal)"))
  } else if (imc >= 25 & imc <= 29.9) {
    return(paste("IMC:", imc, "(sobrepeso)"))
  } else {
    return(paste("IMC:", imc, "(obesidade)"))
  }
}
peso <- 70
altura <- 1.75
cat(calcula_imc(peso, altura), "\n")


