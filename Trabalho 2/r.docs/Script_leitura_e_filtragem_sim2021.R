library(foreign)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

# SIM <- read.dbf("Trabalho 2/Bancos/DO21OPEN.dbf")

munpara <- read_excel("Trabalho 2/Bancos/munpara.xlsx")

munper <- read_excel("Trabalho 2/Bancos/munper.xlsx")

SIM <- SIM %>%
  select(CODMUNRES,IDADE,SEXO,LINHAA)

SIM$CODMUNRES <- as.character(SIM$CODMUNRES)

vmunpara <- as.vector(munpara$CODMUNRES)
vmunpara <- as.character(vmunpara)


# O Código abaixo não funcionou
SIMPA <- SIM %>%
  filter(CODMUNRES %in% vmunpara)

#Salvando o SIM 2021 filtrado para as variáveis que utilizaremos para rodar melhor

write_xlsx(SIM,"Trabalho 2/Bancos/SIM2021.xlsx")
# Não funciona pois o excel tem limite de 1milhão de entradas, tentando em RDS

saveRDS(SIM, file = "SIM2021.rds")
#Deu. 
#Para ler o banco, basta usar o comando:
SIM <- readRDS(file = "Trabalho 2/Bancos/SIM2021.rds")
