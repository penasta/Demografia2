###
### CARREGAR A PLANILHA RDS "POPULACAO"
###

ptogo <- populacao[5:6,]
pecuador <- populacao[26:27,]
pgreece <- populacao[42:43,]

ptogo$`Region, subregion, country or area *` <- NULL
ptogo$`Reference date (as of 1 July)` <- NULL

ptogo[3,] <- ptogo[2,] - ptogo[1,]

ptogo <- ptogo[3,]

pecuador$`Region, subregion, country or area *` <- NULL
pecuador$`Reference date (as of 1 July)` <- NULL

pecuador[3,] <- pecuador[2,] - pecuador[1,]

pecuador <- pecuador[3,]

pgreece$`Region, subregion, country or area *` <- NULL
pgreece$`Reference date (as of 1 July)` <- NULL

pgreece[3,] <- pgreece[2,] - pgreece[1,]

pgreece <- pgreece[3,]

rm(mecuador)
rm(mgreece)
rm(mortalidade)
rm(mtogo)
rm(populacao)

saveRDS(pecuador,file="TCNecuador.rds")
saveRDS(pgreece,file="TCNgreece.rds")
saveRDS(ptogo,file="TCNtogo.rds")