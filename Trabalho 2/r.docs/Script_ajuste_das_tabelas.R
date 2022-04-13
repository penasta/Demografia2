library(readxl)
library(writexl)

PFEM <- read_excel("Trabalho 2/Bancos/Tabelas/Para - Obito por ocorrencia cid10 segundo faixa etaria FEMININA.xlsx")

PFEM[PFEM == "-"] <- NA
PFEM$`Cap V`<- as.numeric(PFEM$`Cap V`)
PFEM$`Cap VIII`<- as.numeric(PFEM$`Cap VIII`)
PFEM$`Cap XII`<- as.numeric(PFEM$`Cap XII`)
PFEM$`Cap XIII`<- as.numeric(PFEM$`Cap XIII`)
PFEM$`Cap XV`<- as.numeric(PFEM$`Cap XV`)
PFEM$`Cap XVI`<- as.numeric(PFEM$`Cap XVI`)
PFEM[is.na(PFEM)] <- 0

PFEM2 <- PFEM[,-1]
rownames(PFEM2) <- PFEM[,1]

PFEM <- PFEM2
rm(PFEM2)

PFEM[1,] <- PFEM[1,]+PFEM[2,]

PFEM[2,] <- NA
PFEM<-na.omit(PFEM)

write_xlsx(PFEM,"Trabalho 2/Bancos/Tabelas/Para - Obito por ocorrencia cid10 segundo faixa etaria FEMININA.xlsx")


#####


PFEM <- read_excel("Trabalho 2/Bancos/Tabelas/Pernambuco - Obito por ocorrencia cid10 segundo faixa etaria FEMININA.xlsx")

PFEM[PFEM == "-"] <- NA
summary(PFEM)
PFEM$`Cap V`<- as.numeric(PFEM$`Cap V`)
PFEM$`Cap VII`<- as.numeric(PFEM$`Cap VII`)
PFEM$`Cap VIII`<- as.numeric(PFEM$`Cap VIII`)
PFEM$`Cap XII`<- as.numeric(PFEM$`Cap XII`)
PFEM$`Cap XIII`<- as.numeric(PFEM$`Cap XIII`)
PFEM$`Cap XV`<- as.numeric(PFEM$`Cap XV`)
PFEM$`Cap XVI`<- as.numeric(PFEM$`Cap XVI`)
PFEM[is.na(PFEM)] <- 0

PFEM2 <- PFEM[,-1]
rownames(PFEM2) <- PFEM[,1]

PFEM <- PFEM2
rm(PFEM2)

PFEM[1,] <- PFEM[1,]+PFEM[2,]

PFEM[2,] <- NA
PFEM<-na.omit(PFEM)

write_xlsx(PFEM,"Trabalho 2/Bancos/Tabelas/Pernambuco - Obito por ocorrencia cid10 segundo faixa etaria FEMININA.xlsx")



#####


PFEM <- read_excel("Trabalho 2/Bancos/Tabelas/Pernambuco - Obito por ocorrencia cid10 segundo faixa etaria MASCULINA.xlsx")

PFEM[PFEM == "-"] <- NA
summary(PFEM)
PFEM$`Cap V`<- as.numeric(PFEM$`Cap V`)
PFEM$`Cap VII`<- as.numeric(PFEM$`Cap VII`)
PFEM$`Cap VIII`<- as.numeric(PFEM$`Cap VIII`)
PFEM$`Cap XII`<- as.numeric(PFEM$`Cap XII`)
PFEM$`Cap XIII`<- as.numeric(PFEM$`Cap XIII`)
PFEM$`Cap XV`<- as.numeric(PFEM$`Cap XV`)
PFEM$`Cap XVI`<- as.numeric(PFEM$`Cap XVI`)
PFEM$`Cap XIV`<- as.numeric(PFEM$`Cap XIV`)
PFEM[is.na(PFEM)] <- 0

PFEM2 <- PFEM[,-1]
rownames(PFEM2) <- PFEM[,1]

PFEM <- PFEM2
rm(PFEM2)

PFEM[1,] <- PFEM[1,]+PFEM[2,]

PFEM[2,] <- NA
PFEM<-na.omit(PFEM)

write_xlsx(PFEM,"Trabalho 2/Bancos/Tabelas/Pernambuco - Obito por ocorrencia cid10 segundo faixa etaria MASCULINA.xlsx")



#####


PFEM <- read_excel("Trabalho 2/Bancos/Tabelas/Para - Obito por ocorrencia cid10 segundo faixa etaria MASCULINA.xlsx")

PFEM[PFEM == "-"] <- NA
summary(PFEM)
PFEM$`Cap V`<- as.numeric(PFEM$`Cap V`)
PFEM$`Cap VII`<- as.numeric(PFEM$`Cap VII`)
PFEM$`Cap VIII`<- as.numeric(PFEM$`Cap VIII`)
PFEM$`Cap XII`<- as.numeric(PFEM$`Cap XII`)
PFEM$`Cap XIII`<- as.numeric(PFEM$`Cap XIII`)
PFEM$`Cap XV`<- as.numeric(PFEM$`Cap XV`)
PFEM$`Cap XVI`<- as.numeric(PFEM$`Cap XVI`)
PFEM$`Cap XIV`<- as.numeric(PFEM$`Cap XIV`)
PFEM[is.na(PFEM)] <- 0

PFEM2 <- PFEM[,-1]
rownames(PFEM2) <- PFEM[,1]

PFEM <- PFEM2
rm(PFEM2)

PFEM[1,] <- PFEM[1,]+PFEM[2,]

PFEM[2,] <- NA
PFEM<-na.omit(PFEM)

write_xlsx(PFEM,"Trabalho 2/Bancos/Tabelas/Para - Obito por ocorrencia cid10 segundo faixa etaria MASCULINA.xlsx")
