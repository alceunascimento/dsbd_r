# QUESTAO 1 ----

library(tibble)

# Criando o tibble com os dados fornecidos na figura
dados <- tibble(
  d = c(10, 40, 70, 10, 45, 75, 15, 50, 80, 20, 50, 85, 25, 55, 90, 30, 60, 90, 30, 65, 35, 70),
  t = c(0.24, 0.36, 0.46, 0.25, 0.40, 0.48, 0.28, 0.43, 0.51, 0.29, 0.41, 0.52, 0.33, 0.45, 0.52, 0.33, 0.44, 0.55, 0.33, 0.46, 0.36, 0.48)
)

# Exibindo o tibble
print(dados)
plot(dados$d ~ dados$t)
plot(dados)

dados$sqrtd <- sqrt(dados$d)

plot(dados$t ~ dados$sqrtd, col = "black", pch = 19)


# base plot
plot(dados$t ~ dados$sqrtd, col = "black", pch = 19)

# modelo (1) y = beta * x
dados$beta <- dados$t / dados$sqrtd
beta1 <- mean(dados$beta)
curve(beta1 * x, add = TRUE, col = "grey", lwd = 2)

# modelo (2) y = beta * x + erro
soma_produto_xy <- sum(dados$sqrtd * dados$t)
soma_x2 <- sum(dados$sqrtd^2)
beta2 <- soma_produto_xy / soma_x2
abline(a = 0, b = beta2, col = "green", lwd = 2)

# modelo (3) regressao linear
modelo <- lm(dados$t ~ dados$sqrtd)
abline(modelo, col = "blue", lwd = 2)
summary(modelo)

modelo$coefficients[2]

g_estimado <- 2 / (modelo$coefficients[2]^2)
g_estimado

confint(modelo)

# Obtenção do intervalo de confiança para o coeficiente sqrt(2/g)
confint_coef <- confint(modelo)[2, ]

# Cálculo do intervalo de confiança para g
ci_g_lower <- 2 / (confint_coef[2]^2)  # Limite inferior
ci_g_upper <- 2 / (confint_coef[1]^2)  # Limite superior

# Exibindo o intervalo de confiança para g
ci_g <- c(ci_g_lower, ci_g_upper)
names(ci_g) <- c("Lower", "Upper")
ci_g


# equacao de Newton
g <- 981  # cm/s^2
newton_t <- sqrt(2/g) * dados$sqrtd
lines(dados$sqrtd, newton_t, col = "red", lwd = 2)


# Ajuste para o gráfico iniciar em (0, 0)
plot(dados$t ~ dados$sqrtd, col = "black", pch = 19, 
     xlab = "Sqrt(d)", ylab = "t", 
     xlim = c(0, max(dados$sqrtd)), ylim = c(0, max(dados$t)))
curve(beta1 * x, add = TRUE, col = "grey", lwd = 2)
abline(a = 0, b = beta2, col = "green", lwd = 2)
abline(modelo, col = "blue", lwd = 2)
lines(dados$sqrtd, newton_t, col = "red", lwd = 2)




# SOLUCAO 1 ----









# QUESTAO 4 ----
# Neste exercício exploramos as ideias centrais da inferencia por verossimilhança.
# Considere que foram coletados dados sobre o tempo de vida (em min.) de um vírus quando fora de uma célula.
# 5.33, 7.16, 1.18, 11.57, 4.75, 9.42, 23.33, 7.18, 17.42, 20.09, 0.53, 4.04, 3.82, 3.24 and 20.91

# Após avaliar opções decidiu-se adotar a distribuição exponencial para descrever a variável.
# O objetivo é fazer inferências utilizando a função de verossimilhança.
# A figura a seguir resume as informações para tal inferência e contém os elementos mostrados em figuras semelhantes nos materiais do curso.

# Baseando-se na figura, marque a(s) afirmativa(s) verdadeira(s).
# (OBS: No texto e figura utiliza-se “.” (ponto) como separador de decimais.)

# Questão Escolha uma ou mais:
  
# a. O intervalo de confiança indicado na figura é simétrico ao redor da estimativa do parâmetro.
# b. Deseja-se testar estatisticamente a hipótese de que o valor do parâmetro difere de 0.158. Utiliza-se o mesmo nível de confiança definido para os intervalos. A conclusão é a de que a hipótese nula de θ=0.158 deve ser: rejeitada.
# c. O intervalo de confiança para o parâmetro é [0.107 ; 0.171].
# d. O valor estimado do parâmetro é de 0.107.
# e. Caso houvesse interesse em obter um intervalo com um nivel de confiança maior do que o utilizado no gráfico, o intervalo seria mais largo do que o mostrado no gráfico.


# SOLUCAO ----

# Dados fornecidos sobre o tempo de vida do vírus (em minutos)
tempos_vida <- c(5.33, 7.16, 1.18, 11.57, 4.75, 9.42, 23.33, 7.18, 17.42, 20.09, 0.53, 4.04, 3.82, 3.24, 20.91)

hist(tempos_vida, breaks = 10, probability = TRUE, main = "Histogram of data")
lines(density(tempos_vida), col="red", lwd=3)


# Função de verossimilhança para a distribuição exponencial
log_verossimilhanca <- function(theta, dados) {
  n <- length(dados)
  soma_dados <- sum(dados)
  n * log(theta) - theta * soma_dados
}

# Encontrar a estimativa máxima de verossimilhança (MLE) para o parâmetro theta
optim_result <- optim(par = 1/mean(tempos_vida), 
                      fn = log_verossimilhanca, 
                      dados = tempos_vida, 
                      control = list(fnscale = -1), # maximização
                      hessian = TRUE)

# Estimativa do parâmetro theta (MLE)
theta_mle <- optim_result$par

# Valor da função de verossimilhança máxima
log_lik_max <- optim_result$value

# Obtenção do intervalo de confiança por método aproximado (assumindo normalidade do MLE)
alpha <- 0.05
z_alpha <- qnorm(1 - alpha / 2)
se_theta <- sqrt(1 / (-optim_result$hessian)) # erro padrão do MLE

# Limites do intervalo de confiança
ic_lower <- theta_mle - z_alpha * se_theta
ic_upper <- theta_mle + z_alpha * se_theta

# Plot da função de verossimilhança
theta_vals <- seq(0.05, 0.20, length.out = 100)
log_lik_vals <- sapply(theta_vals, log_verossimilhanca, dados = tempos_vida)

plot(theta_vals, log_lik_vals, type = "l", lwd = 2,
     xlab = expression(theta),
     ylab = expression(l(theta)),
     main = "Função de Verossimilhança")

# Adicionando os valores do MLE e intervalo de confiança no gráfico
abline(v = theta_mle, col = "blue", lty = 2)
abline(h = log_lik_max - z_alpha^2 / 2, col = "red", lty = 2)
arrows(ic_lower, log_lik_max - z_alpha^2 / 2, ic_lower, min(log_lik_vals), length = 0.1, col = "black")
arrows(ic_upper, log_lik_max - z_alpha^2 / 2, ic_upper, min(log_lik_vals), length = 0.1, col = "black")
text(theta_mle, log_lik_max, labels = round(theta_mle, 3), pos = 3, col = "blue")
text(ic_lower, log_lik_max - z_alpha^2 / 2, labels = round(ic_lower, 3), pos = 1, col = "black")
text(ic_upper, log_lik_max - z_alpha^2 / 2, labels = round(ic_upper, 3), pos = 1, col = "black")

# Resultados
cat("Estimativa pontual (MLE) de theta:", round(theta_mle, 3), "\n")
cat("Intervalo de confiança para theta: [", round(ic_lower, 3), ";", round(ic_upper, 3), "]\n")
