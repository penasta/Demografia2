library(readxl)
nfx <- read_excel("Bancos/Age-specific fertility rates, Total fertility and .xls")
View(nfx)

# 3x

names(nfx) <- nfx[1,]
nfx <- nfx[-1,]

# Descendo o título [após, rodar +1 o código acima]

fix(nfx)

# ecuador 287-291
# greece 377-381
# togo 958-961

nfx2 <- nfx[c(287:291, 377:381, 958:961), ]

# censo togo: 11
# censo grécia: na (10)
# censo equador: 4 

nfx3 <- nfx2[c(4,10,11), ]

fix(nfx3)

summary(nfx3)

n <- nfx3

n$`2` <- NULL
n$`3` <- NULL
n$Period <- NULL
n$`15` <- NULL
n$`16` <- NULL
n$`17` <- NULL
n$`18` <- NULL
n$`19` <- NULL
n$`20` <- NULL
n$`21` <- NULL
n$`22` <- NULL

n$`15-19`<- as.numeric(n$`15-19`)
n$`20-24`<- as.numeric(n$`20-24`)
n$`25-29`<- as.numeric(n$`25-29`)
n$`30-34`<- as.numeric(n$`30-34`)
n$`35-39`<- as.numeric(n$`35-39`)
n$`40-44`<- as.numeric(n$`40-44`)
n$`45-49`<- as.numeric(n$`45-49`)

summary(n)

n$`15-19`<- (n$`15-19`)/1000
n$`20-24`<- (n$`20-24`)/1000
n$`25-29`<- (n$`25-29`)/1000
n$`30-34`<- (n$`30-34`)/1000
n$`35-39`<- (n$`35-39`)/1000
n$`40-44`<- (n$`40-44`)/1000
n$`45-49`<- (n$`45-49`)/1000

saveRDS(n, file = "nfxpaises.rds")