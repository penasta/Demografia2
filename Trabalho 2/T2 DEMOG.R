if (!require(pacman)) {
  install.packages('pacman')
  library(pacman)
}

pacman::p_load(read.dbc,dplyr, stringr, lubridate, LexisPlotR, ggplot2, readxl, Amelia)


# Banco de dados
DOPA19 <- read.dbc("/cloud/project/DOPA2019.dbc")
DOPA20 <- read.dbc("/cloud/project/DOPA2020.dbc")
DOPE19 <- read.dbc("/cloud/project/DOPE2019.dbc")
DOPE20 <- read.dbc("/cloud/project/DOPE2020.dbc")


# Ano
DOPA19$ANO <- NA
DOPA19$ANO <- '2019'

DOPA20$ANO <- NA
DOPA20$ANO <- '2020'

DOPE19$ANO <- NA
DOPE19$ANO <- '2019'

DOPE20$ANO <- NA
DOPE20$ANO <- '2020'

# Juntando os bancos 

DOPA <- full_join(DOPA19,DOPA20)
DOPE <- full_join(DOPE19,DOPE20)

# Selecionando variáveis

DOPA <- DOPA[,c(7:12,46,88)]
DOPE <- DOPE[,c(7:12,46,88)]

# Funcao para codificar a variavel CAUSABAS por capitulo da CID10
categ_cap_cid10 <- function(x) {
  if (str_detect(x, "A|B")) {
    return("I")
  } else if (str_detect(x, "C|(D0)|(D1)|(D2)|(D3)|(D4)")) {
    return("II")
  } else if (str_detect(x, "(D5)|(D6)|(D7)|(D8)")) {
    return("III")
  } else if (str_detect(x, "E")) {
    return("IV")
  } else if (str_detect(x, "F")) {
    return("V")
  } else if (str_detect(x, "G")) {
    return("VI")
  } else if (str_detect(x, "(H0)|(H1)|(H2)|(H3)|(H4)|(H5)")) {
    return("VII")
  } else if (str_detect(x, "(H6)|(H7)|(H8)|(H9)")) {
    return("VIII")
  } else if (str_detect(x, "I")) {
    return("IX")
  } else if (str_detect(x, "J")) {
    return("X")
  } else if (str_detect(x, "K")) {
    return("XI")
  } else if (str_detect(x, "L")) {
    return("XII")
  } else if (str_detect(x, "M")) {
    return("XIII")
  } else if (str_detect(x, "N")) {
    return("XIV")
  } else if (str_detect(x, "O")) {
    return("XV")
  } else if (str_detect(x, "P")) {
    return("XVI")
  } else if (str_detect(x, "Q")) {
    return("XVII")
  } else if (str_detect(x, "R")) {
    return("XVIII")
  } else if (str_detect(x, "S|T")) {
    return("XIX")
  } else if (str_detect(x, "V|W|X|Y")) {
    return("XX")
  } else if (str_detect(x, "Z")) {
    return("XXI")
  } else {
    return("**")
  }
}

DOPA$capcid10 <- NA
str(DOPA$CAUSABAS)
DOPA$CAUSABAS <- sub("[[:punct:]]", "", DOPA$CAUSABAS)

DOPE$capcid10 <- NA
str(DOPE$CAUSABAS)
DOPE$CAUSABAS <- sub("[[:punct:]]", "", DOPE$CAUSABAS)


# Aplicando a funcao para criar uma nova variavel
DOPA$capcid10 <- sapply(DOPA$CAUSABAS, categ_cap_cid10)
DOPA$capcid10nomes <- DOPA$capcid10
DOPA_copia <- DOPA

DOPE$capcid10 <- sapply(DOPE$CAUSABAS, categ_cap_cid10)
DOPE$capcid10nomes <- DOPE$capcid10
DOPE_copia <- DOPE

# Adicionando os labels dos capitulos
DOPA$capcid10nomes <- factor(DOPA$capcid10nomes, 
                                         levels = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X",
                                                    "XI", "XII", "XIII", "XIV", "XV", "XVI", "XVII", "XVIII", "XIX",
                                                    "XX", "XXI", "**"), 
                                         labels = c("Algumas doenças infecciosas e parasitárias",
                                                    "Neoplasmas (tumores)",
                                                    "Doenças do sangue e dos árgãos hematopoéticos e alguns transtornos imunitários",
                                                    "Doenças endócrinas, nutricionais e metabólicas",
                                                    "Transtornos mentais e comportamentais",
                                                    "doenças do sistema nervoso",
                                                    "doenças do olho e anexos",
                                                    "doenças do ouvido e da apófise mastóide",
                                                    "doenças do aparelho circulatório",
                                                    "doenças do aparelho respiratório",
                                                    "doenças do aparelho digestivo",
                                                    "Doenças da pele e do tecido subcutâneo",
                                                    "Doenças do sistema osteomuscular e do tecido conjuntivo",
                                                    "Doenças do aparelho geniturinário",
                                                    "Gravidez, parto e puerpério",
                                                    "Algumas afecções originadas no período perinatal",
                                                    "Malformações congênitas, deformidades e anomalias cromossômicas",
                                                    "Sintomas, sinais e achados anormais de exames clínicos e de laboratório, não classificados em outra parte",
                                                    "Lesões, envenenamentos e algumas outras consequências de causas externas",
                                                    "Causas externas de morbidade e de mortalidade",
                                                    "Fatores que influenciam o estado de saúde e o contato com os serviços de saúde",
                                                    "CID 10: Revisão não disponível ou não preenchido ou inválido"))


DOPE$capcid10nomes <- factor(DOPE$capcid10nomes, 
                             levels = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X",
                                        "XI", "XII", "XIII", "XIV", "XV", "XVI", "XVII", "XVIII", "XIX",
                                        "XX", "XXI", "**"), 
                             labels = c("Algumas doenças infecciosas e parasitárias",
                                        "Neoplasmas (tumores)",
                                        "Doenças do sangue e dos árgãos hematopoéticos e alguns transtornos imunitários",
                                        "Doenças endócrinas, nutricionais e metabólicas",
                                        "Transtornos mentais e comportamentais",
                                        "doenças do sistema nervoso",
                                        "doenças do olho e anexos",
                                        "doenças do ouvido e da apófise mastóide",
                                        "doenças do aparelho circulatório",
                                        "doenças do aparelho respiratório",
                                        "doenças do aparelho digestivo",
                                        "Doenças da pele e do tecido subcutâneo",
                                        "Doenças do sistema osteomuscular e do tecido conjuntivo",
                                        "Doenças do aparelho geniturinário",
                                        "Gravidez, parto e puerpério",
                                        "Algumas afecções originadas no período perinatal",
                                        "Malformações congênitas, deformidades e anomalias cromossômicas",
                                        "Sintomas, sinais e achados anormais de exames clínicos e de laboratório, não classificados em outra parte",
                                        "Lesões, envenenamentos e algumas outras consequências de causas externas",
                                        "Causas externas de morbidade e de mortalidade",
                                        "Fatores que influenciam o estado de saúde e o contato com os serviços de saúde",
                                        "CID 10: Revisão não disponível ou não preenchido ou inválido"))

# Tratando as demais variaveis

DOPA$RACACOR <- factor(DOPA$RACACOR,levels = c(1, 2, 3, 4, 5), 
                       labels = c("Branca",
                                  "Preta",
                                  "Amarela",
                                  "Parda",
                                  "Indigena"))

DOPA$Sexo<-NA
DOPA$Sexo[DOPA$SEXO == 1]<- "M"
DOPA$Sexo[DOPA$SEXO == 2]<- "F"
DOPA$Sexo[DOPA$SEXO == 0]<- "I"
class(DOPA$Sexo)

DOPE$RACACOR <- factor(DOPE$RACACOR,levels = c(1, 2, 3, 4, 5), 
                       labels = c("Branca",
                                  "Preta",
                                  "Amarela",
                                  "Parda",
                                  "Indigena"))

DOPE$Sexo<-NA
DOPE$Sexo[DOPE$SEXO == 1]<- "M"
DOPE$Sexo[DOPE$SEXO == 2]<- "F"
DOPE$Sexo[DOPE$SEXO == 0]<- "I"
class(DOPE$Sexo)


# Função para idade
sim_idade <- function(x) {
  if(str_sub(x,1,1)<4){
    x = 0}else if(str_sub(x,1,1)==4){
      x = 0+as.numeric(str_sub(x,2))}
  else if(str_sub(x,1,1)==5){
    x = 100+as.numeric(str_sub(x,2))}
  else{ x = NA}
  return(x)
}

DOPA$IDADE <- sim_idade(DOPA$IDADE)
DOPE$IDADE <- sim_idade(DOPE$IDADE)

write.csv(DOPA, file = "DOPA.csv", row.names = FALSE)
write.csv(DOPE, file = "DOPE.csv", row.names = FALSE)

