library(readxl)
library(dplyr)
library(readr)
library(tidyverse)
library(xlsx)

# setwd ("D:/Arquivos/unb2021/Demografia/Ex. 4.3 DEM")
# tabela <- read_excel("D:/Arquivos/unb2021/Demografia/Ex. 4.3 DEM/Mortalidade mundo.xlsx")
# pop <- read_excel("D:/Arquivos/unb2021/Demografia/Ex. 4.3 DEM/populacao.xlsx")

names(tabela) <- tabela[1,]
tabela <- tabela[-1,]
#rodei umas 10x acho

names(pop) <- pop[1,]
pop <- pop[-1,]
#rodei umas 10x acho

summary(tabela)
summary(pop)

colnames(tabela) <- c('index','variant','country','notes','country_code','type','parent_code','period','0-4','5-9','10-14','15-19','20-24',
                      '25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80-84','85-89','90-94','95+')

colnames(pop) <- c('index','variant','country','notes','country_code','type','parent_code','period','0-4','5-9','10-14','15-19','20-24',
                      '25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65-69','70-74','75-79','80-84','85-89','90-94','95-99','100+')

summary(tabela)

teste <- tabela %>%
  filter (country == 'Austria' | country == 'Australia')

popt <- pop %>%
  filter (country == 'Austria' | country == 'Australia')


summary(teste)

teste <- teste %>%
  filter(period == '2015-2020')

popt <- popt %>%
  filter(period == '2015')

# come?a daqui para o trab 1 dem 2

# Preparando a fun??o e^-rt para c?lculo da pop estacion?ria

# inserir o r

# Grecia
# rg <- -0.00768
# Equador
# re <- 0.01154
# Togo
# rt <- 0.03543

# inserir o t [5]

t <- 5

# *exp(-r*t)
  
# Inserir per?odo

# grecia:

r <- -0.00768
t <- 5

mortalidade <- mortalidade %>%
  filter(Period == '2005-2010')

populacao <- populacao %>%
  filter(`Reference date (as of 1 July)` == '2005')

mortalidade <- mortalidade %>%
  filter (`Region, subregion, country or area *` == 'Greece')

populacao <- populacao %>%
  filter (`Region, subregion, country or area *` == 'Greece')




populacao$`0-4` <- populacao$`0-4`*1000*exp(-r*t)
populacao$`5-9` <- populacao$`5-9`*1000*exp(-r*t)
populacao$`10-14` <- populacao$`10-14`*1000*exp(-r*t)
populacao$`15-19` <- populacao$`15-19`*1000*exp(-r*t)
populacao$`20-24` <- populacao$`20-24`*1000*exp(-r*t)
populacao$`25-29` <- populacao$`25-29`*1000*exp(-r*t)
populacao$`30-34` <- populacao$`30-34`*1000*exp(-r*t)
populacao$`35-39` <- populacao$`35-39`*1000*exp(-r*t)
populacao$`40-44` <- populacao$`40-44`*1000*exp(-r*t)
populacao$`45-49` <- populacao$`45-49`*1000*exp(-r*t)
populacao$`50-54` <- populacao$`50-54`*1000*exp(-r*t)
populacao$`55-59` <- populacao$`55-59`*1000*exp(-r*t)
populacao$`60-64` <- populacao$`60-64`*1000*exp(-r*t)
populacao$`65-69` <- populacao$`65-69`*1000*exp(-r*t)
populacao$`70-74` <- populacao$`70-74`*1000*exp(-r*t)
populacao$`75-79` <- populacao$`75-79`*1000*exp(-r*t)
populacao$`80-84` <- populacao$`80-84`*1000*exp(-r*t)
populacao$`85-89` <- populacao$`85-89`*1000*exp(-r*t)
populacao$`90-94` <- populacao$`90-94`*1000*exp(-r*t)
populacao$`95+` <- populacao$`95+`*1000*exp(-r*t)

mortalidade$`0-4` <- mortalidade$`0-4`*1000*exp(-r*t)
mortalidade$`5-9` <- mortalidade$`5-9`*1000*exp(-r*t)
mortalidade$`10-14` <- mortalidade$`10-14`*1000*exp(-r*t)
mortalidade$`15-19` <- mortalidade$`15-19`*1000*exp(-r*t)
mortalidade$`20-24` <- mortalidade$`20-24`*1000*exp(-r*t)
mortalidade$`25-29` <- mortalidade$`25-29`*1000*exp(-r*t)
mortalidade$`30-34` <- mortalidade$`30-34`*1000*exp(-r*t)
mortalidade$`35-39` <- mortalidade$`35-39`*1000*exp(-r*t)
mortalidade$`40-44` <- mortalidade$`40-44`*1000*exp(-r*t)
mortalidade$`45-49` <- mortalidade$`45-49`*1000*exp(-r*t)
mortalidade$`50-54` <- mortalidade$`50-54`*1000*exp(-r*t)
mortalidade$`55-59` <- mortalidade$`55-59`*1000*exp(-r*t)
mortalidade$`60-64` <- mortalidade$`60-64`*1000*exp(-r*t)
mortalidade$`65-69` <- mortalidade$`65-69`*1000*exp(-r*t)
mortalidade$`70-74` <- mortalidade$`70-74`*1000*exp(-r*t)
mortalidade$`75-79` <- mortalidade$`75-79`*1000*exp(-r*t)
mortalidade$`80-84` <- mortalidade$`80-84`*1000*exp(-r*t)
mortalidade$`85-89` <- mortalidade$`85-89`*1000*exp(-r*t)
mortalidade$`90-94` <- mortalidade$`90-94`*1000*exp(-r*t)
mortalidade$`95+` <- mortalidade$`95+`*1000*exp(-r*t)

mortalidade <- mortalidade %>%
  select (`0-4`,`5-9`,`10-14`,`15-19`,`20-24`,`25-29`,`30-34`,`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80-84`,`85-89`,`90-94`,`95+`)

populacao <- populacao %>%
  select (`0-4`,`5-9`,`10-14`,`15-19`,`20-24`,`25-29`,`30-34`,`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80-84`,`85-89`,`90-94`,`95+`)

row.names(mortalidade) <- c("Greece")
row.names(populacao) <- c("Greece")

nMxGreece <- mortalidade / populacao

write_rds(nMxGreece,"nMxGreece.rds")

# Togo

r <- 0.03543
t <- 5

mortalidade <- mortalidade %>%
  filter(Period == '1970-1975')

populacao <- populacao %>%
  filter(`Reference date (as of 1 July)` == '1970')

mortalidade <- mortalidade %>%
  filter (`Region, subregion, country or area *` == 'Togo')

populacao <- populacao %>%
  filter (`Region, subregion, country or area *` == 'Togo')




populacao$`0-4` <- populacao$`0-4`*1000*exp(-r*t)
populacao$`5-9` <- populacao$`5-9`*1000*exp(-r*t)
populacao$`10-14` <- populacao$`10-14`*1000*exp(-r*t)
populacao$`15-19` <- populacao$`15-19`*1000*exp(-r*t)
populacao$`20-24` <- populacao$`20-24`*1000*exp(-r*t)
populacao$`25-29` <- populacao$`25-29`*1000*exp(-r*t)
populacao$`30-34` <- populacao$`30-34`*1000*exp(-r*t)
populacao$`35-39` <- populacao$`35-39`*1000*exp(-r*t)
populacao$`40-44` <- populacao$`40-44`*1000*exp(-r*t)
populacao$`45-49` <- populacao$`45-49`*1000*exp(-r*t)
populacao$`50-54` <- populacao$`50-54`*1000*exp(-r*t)
populacao$`55-59` <- populacao$`55-59`*1000*exp(-r*t)
populacao$`60-64` <- populacao$`60-64`*1000*exp(-r*t)
populacao$`65-69` <- populacao$`65-69`*1000*exp(-r*t)
populacao$`70-74` <- populacao$`70-74`*1000*exp(-r*t)
populacao$`75-79` <- populacao$`75-79`*1000*exp(-r*t)
populacao$`80-84` <- populacao$`80-84`*1000*exp(-r*t)
populacao$`85-89` <- populacao$`85-89`*1000*exp(-r*t)
populacao$`90-94` <- populacao$`90-94`*1000*exp(-r*t)
populacao$`95+` <- populacao$`95+`*1000*exp(-r*t)

mortalidade$`0-4` <- mortalidade$`0-4`*1000*exp(-r*t)
mortalidade$`5-9` <- mortalidade$`5-9`*1000*exp(-r*t)
mortalidade$`10-14` <- mortalidade$`10-14`*1000*exp(-r*t)
mortalidade$`15-19` <- mortalidade$`15-19`*1000*exp(-r*t)
mortalidade$`20-24` <- mortalidade$`20-24`*1000*exp(-r*t)
mortalidade$`25-29` <- mortalidade$`25-29`*1000*exp(-r*t)
mortalidade$`30-34` <- mortalidade$`30-34`*1000*exp(-r*t)
mortalidade$`35-39` <- mortalidade$`35-39`*1000*exp(-r*t)
mortalidade$`40-44` <- mortalidade$`40-44`*1000*exp(-r*t)
mortalidade$`45-49` <- mortalidade$`45-49`*1000*exp(-r*t)
mortalidade$`50-54` <- mortalidade$`50-54`*1000*exp(-r*t)
mortalidade$`55-59` <- mortalidade$`55-59`*1000*exp(-r*t)
mortalidade$`60-64` <- mortalidade$`60-64`*1000*exp(-r*t)
mortalidade$`65-69` <- mortalidade$`65-69`*1000*exp(-r*t)
mortalidade$`70-74` <- mortalidade$`70-74`*1000*exp(-r*t)
mortalidade$`75-79` <- mortalidade$`75-79`*1000*exp(-r*t)
mortalidade$`80-84` <- mortalidade$`80-84`*1000*exp(-r*t)
mortalidade$`85-89` <- mortalidade$`85-89`*1000*exp(-r*t)
mortalidade$`90-94` <- mortalidade$`90-94`*1000*exp(-r*t)
mortalidade$`95+` <- mortalidade$`95+`*1000*exp(-r*t)

mortalidade <- mortalidade %>%
  select (`0-4`,`5-9`,`10-14`,`15-19`,`20-24`,`25-29`,`30-34`,`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80-84`,`85-89`,`90-94`,`95+`)

populacao <- populacao %>%
  select (`0-4`,`5-9`,`10-14`,`15-19`,`20-24`,`25-29`,`30-34`,`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80-84`,`85-89`,`90-94`,`95+`)

row.names(mortalidade) <- c("Togo")
row.names(populacao) <- c("Togo")

nMxTogo <- mortalidade / populacao

write_rds(nMxTogo,"nMxTogo.rds")

# Ecuador

r <- 0.01154
t <- 5

mortalidade <- mortalidade %>%
  filter(Period == '2000-2005')

populacao <- populacao %>%
  filter(`Reference date (as of 1 July)` == '2000')

mortalidade <- mortalidade %>%
  filter (`Region, subregion, country or area *` == 'Ecuador')

populacao <- populacao %>%
  filter (`Region, subregion, country or area *` == 'Ecuador')




populacao$`0-4` <- populacao$`0-4`*1000*exp(-r*t)
populacao$`5-9` <- populacao$`5-9`*1000*exp(-r*t)
populacao$`10-14` <- populacao$`10-14`*1000*exp(-r*t)
populacao$`15-19` <- populacao$`15-19`*1000*exp(-r*t)
populacao$`20-24` <- populacao$`20-24`*1000*exp(-r*t)
populacao$`25-29` <- populacao$`25-29`*1000*exp(-r*t)
populacao$`30-34` <- populacao$`30-34`*1000*exp(-r*t)
populacao$`35-39` <- populacao$`35-39`*1000*exp(-r*t)
populacao$`40-44` <- populacao$`40-44`*1000*exp(-r*t)
populacao$`45-49` <- populacao$`45-49`*1000*exp(-r*t)
populacao$`50-54` <- populacao$`50-54`*1000*exp(-r*t)
populacao$`55-59` <- populacao$`55-59`*1000*exp(-r*t)
populacao$`60-64` <- populacao$`60-64`*1000*exp(-r*t)
populacao$`65-69` <- populacao$`65-69`*1000*exp(-r*t)
populacao$`70-74` <- populacao$`70-74`*1000*exp(-r*t)
populacao$`75-79` <- populacao$`75-79`*1000*exp(-r*t)
populacao$`80-84` <- populacao$`80-84`*1000*exp(-r*t)
populacao$`85-89` <- populacao$`85-89`*1000*exp(-r*t)
populacao$`90-94` <- populacao$`90-94`*1000*exp(-r*t)
populacao$`95+` <- populacao$`95+`*1000*exp(-r*t)

mortalidade$`0-4` <- mortalidade$`0-4`*1000*exp(-r*t)
mortalidade$`5-9` <- mortalidade$`5-9`*1000*exp(-r*t)
mortalidade$`10-14` <- mortalidade$`10-14`*1000*exp(-r*t)
mortalidade$`15-19` <- mortalidade$`15-19`*1000*exp(-r*t)
mortalidade$`20-24` <- mortalidade$`20-24`*1000*exp(-r*t)
mortalidade$`25-29` <- mortalidade$`25-29`*1000*exp(-r*t)
mortalidade$`30-34` <- mortalidade$`30-34`*1000*exp(-r*t)
mortalidade$`35-39` <- mortalidade$`35-39`*1000*exp(-r*t)
mortalidade$`40-44` <- mortalidade$`40-44`*1000*exp(-r*t)
mortalidade$`45-49` <- mortalidade$`45-49`*1000*exp(-r*t)
mortalidade$`50-54` <- mortalidade$`50-54`*1000*exp(-r*t)
mortalidade$`55-59` <- mortalidade$`55-59`*1000*exp(-r*t)
mortalidade$`60-64` <- mortalidade$`60-64`*1000*exp(-r*t)
mortalidade$`65-69` <- mortalidade$`65-69`*1000*exp(-r*t)
mortalidade$`70-74` <- mortalidade$`70-74`*1000*exp(-r*t)
mortalidade$`75-79` <- mortalidade$`75-79`*1000*exp(-r*t)
mortalidade$`80-84` <- mortalidade$`80-84`*1000*exp(-r*t)
mortalidade$`85-89` <- mortalidade$`85-89`*1000*exp(-r*t)
mortalidade$`90-94` <- mortalidade$`90-94`*1000*exp(-r*t)
mortalidade$`95+` <- mortalidade$`95+`*1000*exp(-r*t)

mortalidade <- mortalidade %>%
  select (`0-4`,`5-9`,`10-14`,`15-19`,`20-24`,`25-29`,`30-34`,`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80-84`,`85-89`,`90-94`,`95+`)

populacao <- populacao %>%
  select (`0-4`,`5-9`,`10-14`,`15-19`,`20-24`,`25-29`,`30-34`,`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80-84`,`85-89`,`90-94`,`95+`)

row.names(mortalidade) <- c("Ecuador")
row.names(populacao) <- c("Ecuador")

nMxEcuador <- mortalidade / populacao

write_rds(nMxEcuador,"nMxEcuador.rds")

#=========================================================#
nMx <- rbind(nMxGreece, nMxTogo,nMxEcuador)
nmx <- nMx*-1

write_rds(nmx,"nmx.rds")


poptot <- rowSums(populacao)
poptot <- as.data.frame(poptot)
colnames(poptot) <- ('total')
poptot[1,1]  #Australia
poptot[2,1]  #Austria

ncxaustralia <- populacao / poptot[1,1]
ncxaustralia <- ncxaustralia[1,]

ncxaustria <- populacao / poptot[2,1]
ncxaustria <- ncxaustria[2,]
ncxaustria

ncx <- rbind.data.frame(ncxaustralia,ncxaustria)

nmxaustralia <- nMx2015[1,]
nmxaustria <- nMx2015[2,]

tbmaustralia <- nmxaustralia *  ncxaustralia
tbmaustria <- nmxaustria * ncxaustria

AUSTRALIATBM <- sum(tbmaustralia*1000)
AUSTRIATBM <- sum(tbmaustria*1000)
