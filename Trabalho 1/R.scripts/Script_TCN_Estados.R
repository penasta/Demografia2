library(readxl)

poppa <- read_excel("Bancos/Planilhas p calcular crescimento natural absoluto/estados/projecao populacao para pernambuco 2010-2060.xlsx",sheet='PA')
poppe <- read_excel("Bancos/Planilhas p calcular crescimento natural absoluto/estados/projecao populacao para pernambuco 2010-2060.xlsx",sheet='PE')

poppe <- poppe[50:70,]

names(poppe) <- poppe[1,]
poppe <- poppe[-1,]

poppe <- poppe[2:20,]

poppe <- poppe[,1:7]

poppe <- poppe[,c(1,2,7)]

poppec <- poppe

poppe[,4] <- poppe[,3] - poppe[,2]

poppe <- poppe[,4]

row.names(poppe) <- poppec$`GRUPO ETÁRIO`

poppa <- poppa[50:70,]

names(poppa) <- poppa[1,]
poppa <- poppa[-1,]

poppa <- poppa[2:20,]

poppa <- poppa[,c(1,2,7)]

poppac <- poppa

poppa[,4] <- poppa[,3] - poppa[,2]

poppa <- poppa[,4]

row.names(poppa) <- poppac$`GRUPO ETÁRIO`

rm(poppac)
rm(poppec)

saveRDS(poppa,file="TCNpara.rds")
saveRDS(poppe,file="TCNpernambuco.rds")