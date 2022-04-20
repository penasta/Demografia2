library(dplyr)
library(reshape2)

##########
# Carregando e ajeitando o banco de projeções

Proj_M_PA <- readxl::read_xls("Trabalho 2/Bancos/PROJECOES_2013_POPULACAO.xls", range=c("A5:W25"),sheet="PA")
Proj_M_PA$SEXO <- "M"
Proj_M_PE <- readxl::read_xls("Trabalho 2/Bancos/PROJECOES_2013_POPULACAO.xls", range=c("A5:W25"),sheet="PE")
Proj_M_PE$SEXO <- "M"
  
Proj_F_PA <- readxl::read_xls("Trabalho 2/Bancos/PROJECOES_2013_POPULACAO.xls", range=c("A28:W48"),sheet="PA")
Proj_F_PE <- readxl::read_xls("Trabalho 2/Bancos/PROJECOES_2013_POPULACAO.xls", range=c("A28:W48"),sheet="PE")
Proj_F_PA$SEXO <- "F"
Proj_F_PE$SEXO <- "F"

projpa <- rbind(Proj_M_PA,Proj_F_PA)
projpe <- rbind(Proj_M_PE,Proj_F_PE)
projpa$UF <- "PA"
projpe$UF <- "PE"

projpa <- projpa %>%
  filter (`GRUPO ETÁRIO` != "Total")

projpe <- projpe %>%
  filter (`GRUPO ETÁRIO` != "Total")

projpe <- projpe %>%
  select (`GRUPO ETÁRIO`,"2019","2020","2021",SEXO,UF)

projpa <- projpa %>%
  select (`GRUPO ETÁRIO`,"2019","2020","2021",SEXO,UF)


projpa <- melt(projpa,id.vars = c('GRUPO ETÁRIO','UF','SEXO'))
projpe <- melt(projpe,id.vars = c('GRUPO ETÁRIO','UF','SEXO'))

##########
# Carregando o banco de óbitos

obpa <- readxl::read_xlsx("Trabalho 2/Bancos/obpa.xlsx")
obpe <- readxl::read_xlsx("Trabalho 2/Bancos/obpe.xlsx")

##########
# Juntando os bancos

projpa <- bind_cols(projpa, obpa)

colnames(projpa) <- c('idade', 'uf','sexo','ano','pop','obitos')

projpe <- bind_cols(projpe, obpe)

colnames(projpe) <- c('idade', 'uf','sexo','ano','pop','obitos')

##########
# Agregando os três anos para construir uma média móvel

projpa$idade[projpa$idade=="5-9"] <- "05-09"

projpe$idade[projpe$idade=="5-9"] <- "05-09"

projpa <- projpa %>% 
  group_by(uf,sexo,idade) %>%
  summarise(across(c("pop", "obitos"), ~ mean(.x, na.rm = TRUE)))

projpe <- projpe %>% 
  group_by(uf,sexo,idade) %>%
  summarise(across(c("pop", "obitos"), ~ mean(.x, na.rm = TRUE)))

projpa$pop <- round(projpa$pop)
projpa$obitos <- round(projpa$obitos)

projpe$pop <- round(projpe$pop)
projpe$obitos <- round(projpe$obitos)

##########
# Criando coluna nMx

projpa <- projpa %>%
  mutate(NMX=round(obitos/pop,7))

projpe <- projpe %>%
  mutate(NMX=round(obitos/pop,7))

##########
# 
library(writexl)
write_xlsx(projpa,"projpa.xlsx")
write_xlsx(projpe,"projpe.xlsx")