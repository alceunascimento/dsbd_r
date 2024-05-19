##########################################################################
####    EXERCICIO IF ELSE
###############################################################################


# Parâmetros
horario <- "manhã"
valor <- 150

# Função para enviar o e-mail
enviar_email <- function(horario, valor_compra) {
  # Verificar o horário da compra e cumprimentar de acordo
  if (horario == "manhã") {
    cumprimento <- "Bom dia,"
    desconto <- ifelse(valor_compra > 100, 0.10, 0.05)
  } else if (horario == "tarde") {
    cumprimento <- "Boa tarde,"
    desconto <- ifelse(valor_compra > 100, 0.10, 0.05)
  } else {
    cumprimento <- "Boa noite,"
    desconto <- ifelse(valor_compra > 100, 0.10, 0.05)
  }
  
  # Montar o corpo do e-mail
  corpo_email <- paste(cumprimento, "\n\n", "Obrigado por sua compra! Como agradecimento, oferecemos um cupom de desconto de", desconto*100, "% para sua próxima compra.\n\nCupom: DESCONTO", desconto*100, "\n\nAtenciosamente,\nEquipe da Empresa")
  
  # Enviar o e-mail (código para envio de e-mail não fornecido)
  # Aqui você precisaria incluir o código para enviar o e-mail, usando alguma biblioteca ou serviço de e-mail em R
  # Exemplo fictício: enviar_email(email_cliente, "Agradecimento pela compra", corpo_email)
  
  # Exemplo fictício de impressão do e-mail
  print(corpo_email)
}

# Chamada da função com os parâmetros fornecidos
enviar_email(horario, valor)
