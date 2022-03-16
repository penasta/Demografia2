library(readxl)
library(dplyr)
library(readr)
library(tidyverse)
library(xlsx)

setwd ("D:/Arquivos/unb2021/Demografia/Ex. 4.3 DEM")
tabela <- read_excel("D:/Arquivos/unb2021/Demografia/Ex. 4.3 DEM/Mortalidade mundo.xlsx")
pop <- read_excel("D:/Arquivos/unb2021/Demografia/Ex. 4.3 DEM/populacao.xlsx")

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

summary(teste)

popt$`95-99` <- as.numeric(popt$`95-99`)
popt$`100+` <- as.numeric(popt$`100+`)

popt$`95+` <- popt$`95-99` + popt$`100+`

teste <- teste %>%
  select (country,`0-4`,`5-9`,`10-14`,`15-19`,`20-24`,`25-29`,`30-34`,`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80-84`,`85-89`,`90-94`,`95+`)

popt <- popt %>%
  select (country,`0-4`,`5-9`,`10-14`,`15-19`,`20-24`,`25-29`,`30-34`,`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80-84`,`85-89`,`90-94`,`95+`)

popt$`0-4` <- as.numeric(popt$`0-4`)
popt$`5-9` <- as.numeric(popt$`5-9`)
popt$`10-14` <- as.numeric(popt$`10-14`)
popt$`15-19` <- as.numeric(popt$`15-19`)
popt$`20-24` <- as.numeric(popt$`20-24`)
popt$`25-29` <- as.numeric(popt$`25-29`)
popt$`30-34` <- as.numeric(popt$`30-34`)
popt$`35-39` <- as.numeric(popt$`35-39`)
popt$`40-44` <- as.numeric(popt$`40-44`)
popt$`45-49` <- as.numeric(popt$`45-49`)
popt$`50-54` <- as.numeric(popt$`50-54`)
popt$`55-59` <- as.numeric(popt$`55-59`)
popt$`60-64` <- as.numeric(popt$`60-64`)
popt$`65-69` <- as.numeric(popt$`65-69`)
popt$`70-74` <- as.numeric(popt$`70-74`)
popt$`75-79` <- as.numeric(popt$`75-79`)
popt$`80-84` <- as.numeric(popt$`80-84`)
popt$`85-89` <- as.numeric(popt$`85-89`)
popt$`90-94` <- as.numeric(popt$`90-94`)
popt$`95+` <- as.numeric(popt$`95+`)

teste$`0-4` <- as.numeric(teste$`0-4`)
teste$`5-9` <- as.numeric(teste$`5-9`)
teste$`10-14` <- as.numeric(teste$`10-14`)
teste$`15-19` <- as.numeric(teste$`15-19`)
teste$`20-24` <- as.numeric(teste$`20-24`)
teste$`25-29` <- as.numeric(teste$`25-29`)
teste$`30-34` <- as.numeric(teste$`30-34`)
teste$`35-39` <- as.numeric(teste$`35-39`)
teste$`40-44` <- as.numeric(teste$`40-44`)
teste$`45-49` <- as.numeric(teste$`45-49`)
teste$`50-54` <- as.numeric(teste$`50-54`)
teste$`55-59` <- as.numeric(teste$`55-59`)
teste$`60-64` <- as.numeric(teste$`60-64`)
teste$`65-69` <- as.numeric(teste$`65-69`)
teste$`70-74` <- as.numeric(teste$`70-74`)
teste$`75-79` <- as.numeric(teste$`75-79`)
teste$`80-84` <- as.numeric(teste$`80-84`)
teste$`85-89` <- as.numeric(teste$`85-89`)
teste$`90-94` <- as.numeric(teste$`90-94`)
teste$`95+` <- as.numeric(teste$`95+`)

write_rds(teste,"mortalidade.rds")
write_rds(popt,"populacao.rds")

populacao$`0-4` <- populacao$`0-4`*1000
populacao$`5-9` <- populacao$`5-9`*1000
populacao$`10-14` <- populacao$`10-14`*1000
populacao$`15-19` <- populacao$`15-19`*1000
populacao$`20-24` <- populacao$`20-24`*1000
populacao$`25-29` <- populacao$`25-29`*1000
populacao$`30-34` <- populacao$`30-34`*1000
populacao$`35-39` <- populacao$`35-39`*1000
populacao$`40-44` <- populacao$`40-44`*1000
populacao$`45-49` <- populacao$`45-49`*1000
populacao$`50-54` <- populacao$`50-54`*1000
populacao$`55-59` <- populacao$`55-59`*1000
populacao$`60-64` <- populacao$`60-64`*1000
populacao$`65-69` <- populacao$`65-69`*1000
populacao$`70-74` <- populacao$`70-74`*1000
populacao$`75-79` <- populacao$`75-79`*1000
populacao$`80-84` <- populacao$`80-84`*1000
populacao$`85-89` <- populacao$`85-89`*1000
populacao$`90-94` <- populacao$`90-94`*1000
populacao$`95+` <- populacao$`95+`*1000

mortalidade$`0-4` <- mortalidade$`0-4`*1000
mortalidade$`5-9` <- mortalidade$`5-9`*1000
mortalidade$`10-14` <- mortalidade$`10-14`*1000
mortalidade$`15-19` <- mortalidade$`15-19`*1000
mortalidade$`20-24` <- mortalidade$`20-24`*1000
mortalidade$`25-29` <- mortalidade$`25-29`*1000
mortalidade$`30-34` <- mortalidade$`30-34`*1000
mortalidade$`35-39` <- mortalidade$`35-39`*1000
mortalidade$`40-44` <- mortalidade$`40-44`*1000
mortalidade$`45-49` <- mortalidade$`45-49`*1000
mortalidade$`50-54` <- mortalidade$`50-54`*1000
mortalidade$`55-59` <- mortalidade$`55-59`*1000
mortalidade$`60-64` <- mortalidade$`60-64`*1000
mortalidade$`65-69` <- mortalidade$`65-69`*1000
mortalidade$`70-74` <- mortalidade$`70-74`*1000
mortalidade$`75-79` <- mortalidade$`75-79`*1000
mortalidade$`80-84` <- mortalidade$`80-84`*1000
mortalidade$`85-89` <- mortalidade$`85-89`*1000
mortalidade$`90-94` <- mortalidade$`90-94`*1000
mortalidade$`95+` <- mortalidade$`95+`*1000

write_rds(mortalidade,"mortalidade.rds")
write_rds(populacao,"populacao.rds")

row.names(mortalidade) <- c("Australia","Austria")
row.names(populacao) <- c("Australia","Austria")

mortalidade <- mortalidade %>%
  select (`0-4`,`5-9`,`10-14`,`15-19`,`20-24`,`25-29`,`30-34`,`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80-84`,`85-89`,`90-94`,`95+`)

populacao <- populacao %>%
  select (`0-4`,`5-9`,`10-14`,`15-19`,`20-24`,`25-29`,`30-34`,`35-39`,`40-44`,`45-49`,`50-54`,`55-59`,`60-64`,`65-69`,`70-74`,`75-79`,`80-84`,`85-89`,`90-94`,`95+`)

row.names(mortalidade) <- c("Australia","Austria")
row.names(populacao) <- c("Australia","Austria")

nMx <- mortalidade / populacao

write_rds(nMx,"nMx.rds")

#=======

nMx2015 <- nMx/5

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
#Padronização direta:
ncxmedia <- (ncxaustralia + ncxaustria)/2


tbmaustraliapd <- nmxaustralia * ncxmedia
TBMPDAUSTRALIA <- sum(tbmaustraliapd*1000)
tbmaustriapd <- nmxaustria * ncxmedia
TBMPDAUSTRIA <- sum(tbmaustriapd*1000)

#Padronização indireta:
nmxmedia <- (nmxaustralia + nmxaustria)/2

tbmaustraliapi <- nmxmedia * ncxaustralia
TBMPIAUSTRALIA <- sum(tbmaustraliapi*1000)
tbmaustriapi <-  nmxmedia * ncxaustria
TBMPIAUSTRIA <- sum(tbmaustriapi*1000)

#Gerando as tabelas
# TBM não padronizada
#Australia
tabelatbmaustralia <- rbind.data.frame(nmxaustralia,ncxaustralia)
row.names(tabelatbmaustralia) <- c("nMx","nCx")
tabelatbmaustralia <- as.data.frame(t(tabelatbmaustralia))
#Austria
tabelatbmaustria <- rbind.data.frame(nmxaustria,ncxaustria)
row.names(tabelatbmaustria) <- c("nMx","nCx")
tabelatbmaustria <- as.data.frame(t(tabelatbmaustria))

# TBM padronização direta
#Australia
tabelatbmpdaustralia <- rbind.data.frame(nmxaustralia,ncxmedia)
row.names(tabelatbmpdaustralia) <- c("nMx","nCx")
tabelatbmpdaustralia <- as.data.frame(t(tabelatbmpdaustralia))
#Austria
tabelatbmpdaustria <- rbind.data.frame(nmxaustria,ncxmedia)
row.names(tabelatbmpdaustria) <- c("nMx","nCx")
tabelatbmpdaustria <- as.data.frame(t(tabelatbmpdaustria))

# TBM padronização indireta
#Australia
tabelatbmpiaustralia <- rbind.data.frame(nmxmedia,ncxaustralia)
row.names(tabelatbmpiaustralia) <- c("nMx","nCx")
tabelatbmpiaustralia <- as.data.frame(t(tabelatbmpiaustralia))
#Austria
tabelatbmpiaustria <- rbind.data.frame(nmxmedia,ncxaustria)
row.names(tabelatbmpiaustria) <- c("nMx","nCx")
tabelatbmpiaustria <- as.data.frame(t(tabelatbmpiaustria))

#Exportando as tabelas para excel
write.xlsx(tabelatbmaustralia,file="TBM_Australia.xlsx")
write.xlsx(tabelatbmaustria,file="TBM_Austria.xlsx")

write.xlsx(tabelatbmpdaustralia,file="TBM_PD_Australia.xlsx")
write.xlsx(tabelatbmpdaustria,file="TBM_PD_Austria.xlsx")

write.xlsx(tabelatbmpiaustralia,file="TBM_PI_Australia.xlsx")
write.xlsx(tabelatbmpiaustria,file="TBM_PI_Austria.xlsx")