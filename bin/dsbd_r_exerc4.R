# tidyverse

rm(list=ls())


# setup
# getting data
library(readr)
library(readxl)
library(haven) 
library(rvest)
library(data.table)

# wrangling data
library(tidyr)
library(forcats)
library(dplyr)
library(dbplyr)
library(dtplyr)
library(lubridate)
library(hms)
library(stringr)
library(tibble)
library(magrittr)


# modeling data
library(purrr)

# tidymodels
library(broom)
library(tune) #
library(recipes)
library(dials)
library(rsample) #
library(parsnip)
library(yardstick)
library(infer)
library(tidyposterior) #
library(workflows)
library(corrr)




# comunicating data
library(markdown)
library(ggplot2)
library(knitr)


# EXERCICIO

data <- readr::read_csv("data/mental.csv")
head(data)
data
dplyr::glimpse(data)


# magrittr
x <- c(-2:2)
x

# Opcao 1
sort(cos(unique(x)), decreasing = TRUE)

# Opcao 2
sort(
  cos(
    unique(
      x
    )
  ),
  decreasing = TRUE)



# Opcao 3
require(magrittr)
x %>% 
  unique() %>% 
  cos() %>% 
  sort(decreasing = TRUE)



# PIPE DE ATRIBUICAO  %<>%
notas <- c("João" = 7.3,
           "Maria" = 5.1,
           "Pedro" = 8.9,
           "Ana" = 6.5)
notas

notas %<>%
  add(5) %>% 
  divide_by(2)
notas



data$mercosul = ifelse(data$Country %in%
                         c("Argentia", "Brazil", "Paraguay", "Uruguay"),
                       "Mercosul", "Não-mercosul")
glimpse(data)


data <-  data %>% 
  mutate(mercosul = ifelse(data$Country %in%
                          c("Argentia", "Brazil", "Paraguay", "Uruguay"),
                        "Mercosul", "Não-mercosul"))
glimpse(data)

data2 <- data %>% 
  select(Country, Timestamp, Days_Indoors, mercosul)
glimpse(data2)


#
library(data.table)
data3 <- fread("data/crashes.csv.gz")
glimpse(data3)


data3 %>% 
  filter(tipo_de_ocorrencia == "sem vítima" & automovel >= 3)

# chamar um intervalo numerico
data3 %>% 
  filter(between(automovel, 3, 5))

data3 %>% 
  filter(tipo_de_ocorrencia %in% c("sem vítima", "com vítima"))

# criando um operador personalizado
`%ni%` <- Negate(`%in%`)

data3 %>% 
  filter(tipo_de_ocorrencia %ni% c("sem vítima", "com vítima"))



data3 %>% 
  rename(automovel_batido = automovel)
glimpse(data3)




