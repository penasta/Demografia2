library (ggplot2)
#library(dplyr)
#library(viridis) 
#library(RColorBrewer)
library(readxl)

# Carregar as planilhas PA e PE
PA <- read_excel("Trabalho 2/Bancos/PA.xlsx")
PE <- read_excel("Trabalho 2/Bancos/PE.xlsx")


#------------------- Pará -----------------------------------------------------

PA <- PA[PA$Sexo %in% c('F', 'M'), ]
PA2019 <- PA[PA$ANO == 2019,]
PA2020 <- PA[PA$ANO == 2020,]
PA2021 <- PA[PA$ANO == 2021,]

ggplot(PA2019, aes(x = capcid10, colour=Sexo, fill=Sexo)) +
  geom_bar()+ 
  ggtitle("Mortalidade por capítulo CID10 por sexo - Pará 2019")


ggplot(PA2020, aes(x = capcid10, colour=Sexo, fill=Sexo)) +
  geom_bar()+ 
  ggtitle("Mortalidade por capítulo CID10 por sexo - Pará 2020")#+ 
#  scale_color_manual(values=c("#D2B69C", "#87C2C2"))+ 
#  scale_fill_manual(values=c("#D2B69C", "#87C2C2"))


ggplot(PA2021, aes(x = capcid10, colour=Sexo, fill=Sexo)) +
  geom_bar()+ 
  ggtitle("Mortalidade por capítulo CID10 por sexo - Pará 2021")

#------------------------------------------------------------------------------

#------------------- Pernambuco -----------------------------------------------

PE <- PE[PE$Sexo %in% c('F', 'M'), ]
PE2019 <- PE[PE$ANO == 2019,]
PE2020 <- PE[PE$ANO == 2020,]
PE2021 <- PE[PE$ANO == 2021,]

ggplot(PE2019, aes(x = capcid10, colour=Sexo, fill=Sexo)) +
  geom_bar()+ 
  ggtitle("Mortalidade por capítulo CID10 por sexo - Pernambuco 2019")


ggplot(PE2020, aes(x = capcid10, colour=Sexo, fill=Sexo)) +
  geom_bar()+ 
  ggtitle("Mortalidade por capítulo CID10 por sexo - Pernambuco 2020")


ggplot(PE2021, aes(x = capcid10, colour=Sexo, fill=Sexo)) +
  geom_bar()+ 
  ggtitle("Mortalidade por capítulo CID10 por sexo - Pernambuco 2021")

#------------------------------------------------------------------------------