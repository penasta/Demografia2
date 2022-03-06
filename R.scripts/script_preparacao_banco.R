library(readxl)
library(tidyverse)

populacao <- read_excel("Bancos/populacao.xlsx")
View(populacao)

names(populacao) <- populacao[1,]
populacao <- populacao[-1,]

teste <- populacao[c(1171:1185, 2701:2715, 3526:3540), ]
populacao <- teste
rm(teste)

populacao$Index <- NULL
populacao$Variant <- NULL
populacao$Notes <- NULL
populacao$`Country code` <- NULL
populacao$Type <- NULL
populacao$`Parent code` <- NULL

populacao$`0-4`<- as.numeric(populacao$`0-4`)
populacao$`5-9`<- as.numeric(populacao$`5-9`)
populacao$`10-14`<- as.numeric(populacao$`10-14`)
populacao$`15-19`<- as.numeric(populacao$`15-19`)
populacao$`20-24`<- as.numeric(populacao$`20-24`)
populacao$`25-29`<- as.numeric(populacao$`25-29`)
populacao$`30-34`<- as.numeric(populacao$`30-34`)
populacao$`35-39`<- as.numeric(populacao$`35-39`)
populacao$`40-44`<- as.numeric(populacao$`40-44`)
populacao$`45-49`<- as.numeric(populacao$`45-49`)
populacao$`50-54`<- as.numeric(populacao$`50-54`)
populacao$`55-59`<- as.numeric(populacao$`55-59`)
populacao$`60-64`<- as.numeric(populacao$`60-64`)
populacao$`65-69`<- as.numeric(populacao$`65-69`)
populacao$`70-74`<- as.numeric(populacao$`70-74`)
populacao$`75-79`<- as.numeric(populacao$`75-79`)
populacao$`80-84`<- as.numeric(populacao$`80-84`)
populacao$`85-89`<- as.numeric(populacao$`85-89`)
populacao$`90-94`<- as.numeric(populacao$`90-94`)
populacao$`95-99`<- as.numeric(populacao$`95-99`)
populacao$`100+`<- as.numeric(populacao$`100+`)

populacao$`0-4`<- populacao$`0-4`*1000
populacao$`5-9`<- populacao$`5-9`*1000
populacao$`10-14`<- populacao$`10-14`*1000
populacao$`15-19`<- populacao$`15-19`*1000
populacao$`20-24`<- populacao$`20-24`*1000
populacao$`25-29`<- populacao$`25-29`*1000
populacao$`30-34`<- populacao$`30-34`*1000
populacao$`35-39`<- populacao$`35-39`*1000
populacao$`40-44`<- populacao$`40-44`*1000
populacao$`45-49`<- populacao$`45-49`*1000
populacao$`50-54`<- populacao$`50-54`*1000
populacao$`55-59`<- populacao$`55-59`*1000
populacao$`60-64`<- populacao$`60-64`*1000
populacao$`65-69`<- populacao$`65-69`*1000
populacao$`70-74`<- populacao$`70-74`*1000
populacao$`75-79`<- populacao$`75-79`*1000
populacao$`80-84`<- populacao$`80-84`*1000
populacao$`85-89`<- populacao$`85-89`*1000
populacao$`90-94`<- populacao$`90-94`*1000
populacao$`95-99`<- populacao$`95-99`*1000
populacao$`100+`<- populacao$`100+`*1000

saveRDS(populacao, file = "Bancos/populacao.rds")