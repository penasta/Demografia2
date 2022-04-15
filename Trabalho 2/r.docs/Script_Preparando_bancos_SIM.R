install.packages('read.dbc')
install.packages("dplyr")
install.packages("tidyr")
install.packages("writexl")

library(read.dbc)
library(dplyr)
library(tidyr)
library(writexl)

SPA19 <- read.dbc("Bancos/SIM/DOPA2019.dbc")
SPA20 <- read.dbc("Bancos/SIM/DOPA2020.dbc")

SPA <- rbind(SPA19,SPA20)

SPA <- SPA %>%
  select(IDADE,SEXO,LINHAA,LINHAB,LINHAC,LINHAD,LINHAII)

SPA$IDADE <- as.numeric(as.character(SPA$IDADE))

SPA$IDADE <- (SPA$IDADE - 400)

SPA$IDADE[SPA$IDADE < 0] <- 0 
SPA$IDADE[SPA$IDADE > 200] <- NA 
SPA$IDADE <- as.integer(SPA$IDADE)

SPA$NSEXO <- SPA$SEXO

SPA$NSEXO <- as.numeric(SPA$NSEXO)
SPA$NSEXO[SPA$NSEXO == 1] <- NA
SPA$NSEXO[SPA$NSEXO == 2] <- "Masculino"
SPA$NSEXO[SPA$NSEXO == 3] <- "Feminino"
SPA$NSEXO <- as.factor(SPA$NSEXO)

SPA <- SPA %>%
  drop_na(NSEXO)
SPA <- SPA %>%
  drop_na(LINHAA)
SPA <- SPA %>%
  drop_na(IDADE)

rm(SPA19)
rm(SPA20)

write_xlsx(SPA, "SIMPARA.xlsx")

####### Aproveitando o código para fazer de pernambuco ######

SPA19 <- read.dbc("Bancos/SIM/DOPE2019.dbc")
SPA20 <- read.dbc("Bancos/SIM/DOPE2020.dbc")

SPA <- rbind(SPA19,SPA20)

SPA <- SPA %>%
  select(IDADE,SEXO,LINHAA,LINHAB,LINHAC,LINHAD,LINHAII)

SPA$IDADE <- as.numeric(as.character(SPA$IDADE))

SPA$IDADE <- (SPA$IDADE - 400)

SPA$IDADE[SPA$IDADE < 0] <- 0 
SPA$IDADE[SPA$IDADE > 200] <- NA 
SPA$IDADE <- as.integer(SPA$IDADE)

SPA$NSEXO <- SPA$SEXO

SPA$NSEXO <- as.numeric(SPA$NSEXO)
SPA$NSEXO[SPA$NSEXO == 1] <- NA
SPA$NSEXO[SPA$NSEXO == 2] <- "Masculino"
SPA$NSEXO[SPA$NSEXO == 3] <- "Feminino"
SPA$NSEXO <- as.factor(SPA$NSEXO)

SPA <- SPA %>%
  drop_na(NSEXO)
SPA <- SPA %>%
  drop_na(LINHAA)
SPA <- SPA %>%
  drop_na(IDADE)

rm(SPA19)
rm(SPA20)

write_xlsx(SPA, "SIMPERNAMBUCO.xlsx")

SIMPARA <- read_excel("Trabalho 2/Bancos/SIM/SIMPARA.xlsx")

SIMPARA <- SIMPARA %>%
  select(IDADE,SEXO,LINHAA)

SIMPERNAMBUCO <- read_excel("Trabalho 2/Bancos/SIM/SIMPERNAMBUCO.xlsx")

SIMPERNAMBUCO <- SIMPERNAMBUCO %>%
  select(IDADE,SEXO,LINHAA)

d <- d %>%
  select(IDADE,SEXO,LINHAA)

t <- t %>%
  select(IDADE,SEXO,LINHAA)

SIMPERNAMBUCO <- rbind(SIMPERNAMBUCO,d)

SIMPARA <- rbind(SIMPARA,t)

write_xlsx(SIMPERNAMBUCO, "SIMPERNAMBUCO.xlsx")
write_xlsx(SIMPARA, "SIMPARA.xlsx")