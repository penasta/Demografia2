install.packages("tidyverse")

library(foreign)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)
library(tidyverse)

# SIM <- read.dbf("Trabalho 2/Bancos/DO21OPEN.dbf")

munpara <- read_excel("Trabalho 2/Bancos/munpara.xlsx")

munper <- read_excel("Trabalho 2/Bancos/munper.xlsx")

# SIM <- SIM %>%
#  select(CODMUNRES,IDADE,SEXO,LINHAA)

SIM$CODMUNRES <- as.character(SIM$CODMUNRES)

t <- SIM
t$CODMUNRES <- as.character(SIM$CODMUNRES)
t$CODMUNRES <- as.numeric(SIM$CODMUNRES)

vmunpara <- as.vector(munpara$CODMUNRES)
vmunpara <- as.character(vmunpara)

SIM$IDADE <- as.numeric(as.character(SIM$IDADE))

SIM$IDADE <- (SIM$IDADE - 400)

SIM$IDADE[SIM$IDADE < 0] <- 0 
SIM$IDADE[SIM$IDADE > 200] <- NA 
SIM$IDADE <- as.integer(SIM$IDADE)

SIM <- SIM %>%
  drop_na(IDADE)


t <- t %>%
  filter(CODMUNRES<159999)

t <- t %>%
  filter(CODMUNRES>150000)

d <- SIM

d <- d %>%
  filter(CODMUNRES<269999)

d <- d %>%
  filter(CODMUNRES>260000)






#Salvando o SIM 2021 filtrado para as variáveis que utilizaremos para rodar melhor
write_xlsx(t,"Trabalho 2/Bancos/SIM2021PARA.xlsx")
write_xlsx(d,"Trabalho 2/Bancos/SIM2021PERNAMBUCO.xlsx")
write_xlsx(SIM,"Trabalho 2/Bancos/SIM2021.xlsx")
# Não funciona pois o excel tem limite de 1milhão de entradas, tentando em RDS

saveRDS(SIM, file = "SIM2021.rds")
#Deu. 
#Para ler o banco, basta usar o comando:
SIM <- readRDS(file = "Trabalho 2/Bancos/SIM2021.rds")
