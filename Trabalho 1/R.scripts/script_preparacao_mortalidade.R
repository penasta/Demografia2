library(readxl)
mort <- read_excel("Bancos/Banco_Mortalidade.xlsx")
View(mort)

names(mort) <- mort[1,]
mort <- mort[-1,]

# togo 1093-1106
# equador 2521-2534
# gre 3291-3304

mort <- mort[c(1093:1106, 2521:2534, 3291:3304), ]

mort$Index <- NULL
mort$Variant <- NULL
mort$Notes <- NULL
mort$`Country code` <- NULL
mort$Type <- NULL
mort$`Parent code`<- NULL

summary(mort)

mort$`0-4`<- as.numeric(mort$`0-4`)
mort$`5-9`<- as.numeric(mort$`5-9`)
mort$`10-14`<- as.numeric(mort$`10-14`)
mort$`15-19`<- as.numeric(mort$`15-19`)
mort$`20-24`<- as.numeric(mort$`20-24`)
mort$`25-29`<- as.numeric(mort$`25-29`)
mort$`30-34`<- as.numeric(mort$`30-34`)
mort$`35-39`<- as.numeric(mort$`35-39`)
mort$`40-44`<- as.numeric(mort$`40-44`)
mort$`45-49`<- as.numeric(mort$`45-49`)
mort$`50-54`<- as.numeric(mort$`50-54`)
mort$`55-59`<- as.numeric(mort$`55-59`)
mort$`60-64`<- as.numeric(mort$`60-64`)
mort$`65-69`<- as.numeric(mort$`65-69`)
mort$`70-74`<- as.numeric(mort$`70-74`)
mort$`75-79`<- as.numeric(mort$`75-79`)
mort$`80-84`<- as.numeric(mort$`80-84`)
mort$`85-89`<- as.numeric(mort$`85-89`)
mort$`90-94`<- as.numeric(mort$`90-94`)
mort$`95+`<- as.numeric(mort$`95+`)

summary(mort)

mort$`0-4`<- mort$`0-4`*1000
mort$`5-9`<- mort$`5-9`*1000
mort$`10-14`<- mort$`10-14`*1000
mort$`15-19`<- mort$`15-19`*1000
mort$`20-24`<- mort$`20-24`*1000
mort$`25-29`<- mort$`25-29`*1000
mort$`30-34`<- mort$`30-34`*1000
mort$`35-39`<- mort$`35-39`*1000
mort$`40-44`<- mort$`40-44`*1000
mort$`45-49`<- mort$`45-49`*1000
mort$`50-54`<- mort$`50-54`*1000
mort$`55-59`<- mort$`55-59`*1000
mort$`60-64`<- mort$`60-64`*1000
mort$`65-69`<- mort$`65-69`*1000
mort$`70-74`<- mort$`70-74`*1000
mort$`75-79`<- mort$`75-79`*1000
mort$`80-84`<- mort$`80-84`*1000
mort$`85-89`<- mort$`85-89`*1000
mort$`90-94`<- mort$`90-94`*1000
mort$`95+`<- mort$`95+`*1000

# togo 1-14
# equador 15-28
# gre 29-42

saveRDS(mort, file = "Bancos/mortalidade.rds")


morteq <- mort[c(15:28),]
mortgr <- mort[c(29:42),]
mortto <- mort[c(1:14),]
