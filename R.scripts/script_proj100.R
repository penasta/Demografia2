library(dplyr)

a2010 <- tabuaPE$pop2010

pop <- tabuaPE %>%
  select ( sexo, idade,pop2015)

a2015 <- tabuaPE$pop2015

pop$sexo = as.character(pop$sexo)
pop$idade = as.integer(pop$idade)
pop$pop2010 = as.numeric(pop$pop2010)

nLx$sexo = as.character(nLx$sexo)
nLx$idade = as.integer(nLx$idade)
nLx$nLx = as.numeric(nLx$nLx)

TEF$ano = as.numeric(TEF$ano)
TEF$idade = as.integer(TEF$idade)
TEF$TEF = as.numeric(TEF$TEF)

nLx[1:17,4] = nLx[2:18,3]/nLx[1:17,3]
nLx[18,4] = nLx[19,3]/(nLx[18,3]+nLx[19,3])
nLx[20:36,4] = nLx[21:37,3]/nLx[20:36,3]
nLx[37,4] = nLx[38,3]/(nLx[37,3]+nLx[38,3])
colnames(nLx) = c('sexo', 'idade', 'nLx', 'nSx')

pop$nSx = nLx$nSx
pop[2:19,5] = round(pop[1:18,3]*pop[1:18,4],0)
pop[19,5] = round((pop[19,3]+pop[18,3])*pop[18,4],0)
pop[21:38,5] = round(pop[20:37,3]*pop[20:37,4],0)
pop[38,5] = round((pop[38,3]+pop[37,3])*pop[37,4],0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada')

pop$TLM <- 0
pop$pop2015aberta = round(pop$pop2015fechada+(pop$pop2015fechada*pop$TLM),0)

pop$TEF <- NA
pop[4:10,8] = TEF$TEF
pop[1,9] = round(2.5*sum((pop[4:10,3]+pop[4:10,7])*pop[4:10,8]),0)
pop$V9 <- NULL

pop$'9' <- NA
pop$'10' <- NA

pop[1,10] = round(pop[1,9]*(1/(1+1.04))*(nLx[1,3]/500000),0)
pop[20,10] = round(pop[1,9]*(1.04/(1+1.04))*(nLx[20,3]/500000),0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada', 'TLM',
                  'pop2015aberta','TEF', 'B.total', 'B.sexo.sobrev')

pop$pop2020 = round(ifelse(is.na(pop$pop2015aberta), pop$B.sexo.sobrev,
                           pop$pop2015aberta),0)

a2020 <- pop$pop2020

pop <- pop %>%
  select ( sexo, idade,pop2020)

pop$sexo = as.character(pop$sexo)
pop$idade = as.integer(pop$idade)
pop$pop2010 = as.numeric(pop$pop2010)

nLx$sexo = as.character(nLx$sexo)
nLx$idade = as.integer(nLx$idade)
nLx$nLx = as.numeric(nLx$nLx)

TEF$ano = as.numeric(TEF$ano)
TEF$idade = as.integer(TEF$idade)
TEF$TEF = as.numeric(TEF$TEF)

nLx[1:17,4] = nLx[2:18,3]/nLx[1:17,3]
nLx[18,4] = nLx[19,3]/(nLx[18,3]+nLx[19,3])
nLx[20:36,4] = nLx[21:37,3]/nLx[20:36,3]
nLx[37,4] = nLx[38,3]/(nLx[37,3]+nLx[38,3])
colnames(nLx) = c('sexo', 'idade', 'nLx', 'nSx')

pop$nSx = nLx$nSx
pop[2:19,5] = round(pop[1:18,3]*pop[1:18,4],0)
pop[19,5] = round((pop[19,3]+pop[18,3])*pop[18,4],0)
pop[21:38,5] = round(pop[20:37,3]*pop[20:37,4],0)
pop[38,5] = round((pop[38,3]+pop[37,3])*pop[37,4],0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada')

pop$TLM <- 0
pop$pop2015aberta = round(pop$pop2015fechada+(pop$pop2015fechada*pop$TLM),0)

pop$TEF <- NA
pop[4:10,8] = TEF$TEF
pop[1,9] = round(2.5*sum((pop[4:10,3]+pop[4:10,7])*pop[4:10,8]),0)
pop$V9 <- NULL

pop$'9' <- NA
pop$'10' <- NA

pop[1,10] = round(pop[1,9]*(1/(1+1.04))*(nLx[1,3]/500000),0)
pop[20,10] = round(pop[1,9]*(1.04/(1+1.04))*(nLx[20,3]/500000),0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada', 'TLM',
                  'pop2015aberta','TEF', 'B.total', 'B.sexo.sobrev')

pop$pop2025 = round(ifelse(is.na(pop$pop2015aberta), pop$B.sexo.sobrev,
                           pop$pop2015aberta),0)

a2025 <- pop$pop2025

pop <- pop %>%
  select ( sexo, idade, pop2025)

pop$sexo = as.character(pop$sexo)
pop$idade = as.integer(pop$idade)
pop$pop2010 = as.numeric(pop$pop2010)

nLx$sexo = as.character(nLx$sexo)
nLx$idade = as.integer(nLx$idade)
nLx$nLx = as.numeric(nLx$nLx)

TEF$ano = as.numeric(TEF$ano)
TEF$idade = as.integer(TEF$idade)
TEF$TEF = as.numeric(TEF$TEF)

nLx[1:17,4] = nLx[2:18,3]/nLx[1:17,3]
nLx[18,4] = nLx[19,3]/(nLx[18,3]+nLx[19,3])
nLx[20:36,4] = nLx[21:37,3]/nLx[20:36,3]
nLx[37,4] = nLx[38,3]/(nLx[37,3]+nLx[38,3])
colnames(nLx) = c('sexo', 'idade', 'nLx', 'nSx')

pop$nSx = nLx$nSx
pop[2:19,5] = round(pop[1:18,3]*pop[1:18,4],0)
pop[19,5] = round((pop[19,3]+pop[18,3])*pop[18,4],0)
pop[21:38,5] = round(pop[20:37,3]*pop[20:37,4],0)
pop[38,5] = round((pop[38,3]+pop[37,3])*pop[37,4],0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada')

pop$TLM <- 0
pop$pop2015aberta = round(pop$pop2015fechada+(pop$pop2015fechada*pop$TLM),0)

pop$TEF <- NA
pop[4:10,8] = TEF$TEF
pop[1,9] = round(2.5*sum((pop[4:10,3]+pop[4:10,7])*pop[4:10,8]),0)
pop$V9 <- NULL

pop$'9' <- NA
pop$'10' <- NA

pop[1,10] = round(pop[1,9]*(1/(1+1.04))*(nLx[1,3]/500000),0)
pop[20,10] = round(pop[1,9]*(1.04/(1+1.04))*(nLx[20,3]/500000),0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada', 'TLM',
                  'pop2015aberta','TEF', 'B.total', 'B.sexo.sobrev')

pop$pop2030 = round(ifelse(is.na(pop$pop2015aberta), pop$B.sexo.sobrev,
                           pop$pop2015aberta),0)

a2030 <- pop$pop2030

pop <- pop %>%
  select ( sexo, idade, pop2030)

pop$sexo = as.character(pop$sexo)
pop$idade = as.integer(pop$idade)
pop$pop2010 = as.numeric(pop$pop2010)

nLx$sexo = as.character(nLx$sexo)
nLx$idade = as.integer(nLx$idade)
nLx$nLx = as.numeric(nLx$nLx)

TEF$ano = as.numeric(TEF$ano)
TEF$idade = as.integer(TEF$idade)
TEF$TEF = as.numeric(TEF$TEF)

nLx[1:17,4] = nLx[2:18,3]/nLx[1:17,3]
nLx[18,4] = nLx[19,3]/(nLx[18,3]+nLx[19,3])
nLx[20:36,4] = nLx[21:37,3]/nLx[20:36,3]
nLx[37,4] = nLx[38,3]/(nLx[37,3]+nLx[38,3])
colnames(nLx) = c('sexo', 'idade', 'nLx', 'nSx')

pop$nSx = nLx$nSx
pop[2:19,5] = round(pop[1:18,3]*pop[1:18,4],0)
pop[19,5] = round((pop[19,3]+pop[18,3])*pop[18,4],0)
pop[21:38,5] = round(pop[20:37,3]*pop[20:37,4],0)
pop[38,5] = round((pop[38,3]+pop[37,3])*pop[37,4],0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada')

pop$TLM <- 0
pop$pop2015aberta = round(pop$pop2015fechada+(pop$pop2015fechada*pop$TLM),0)

pop$TEF <- NA
pop[4:10,8] = TEF$TEF
pop[1,9] = round(2.5*sum((pop[4:10,3]+pop[4:10,7])*pop[4:10,8]),0)
pop$V9 <- NULL

pop$'9' <- NA
pop$'10' <- NA

pop[1,10] = round(pop[1,9]*(1/(1+1.04))*(nLx[1,3]/500000),0)
pop[20,10] = round(pop[1,9]*(1.04/(1+1.04))*(nLx[20,3]/500000),0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada', 'TLM',
                  'pop2015aberta','TEF', 'B.total', 'B.sexo.sobrev')

pop$pop2035 = round(ifelse(is.na(pop$pop2015aberta), pop$B.sexo.sobrev,
                           pop$pop2015aberta),0)

a2035 <- pop$pop2035

pop <- pop %>%
  select ( sexo, idade, pop2035)

pop$sexo = as.character(pop$sexo)
pop$idade = as.integer(pop$idade)
pop$pop2010 = as.numeric(pop$pop2010)

nLx$sexo = as.character(nLx$sexo)
nLx$idade = as.integer(nLx$idade)
nLx$nLx = as.numeric(nLx$nLx)

TEF$ano = as.numeric(TEF$ano)
TEF$idade = as.integer(TEF$idade)
TEF$TEF = as.numeric(TEF$TEF)

nLx[1:17,4] = nLx[2:18,3]/nLx[1:17,3]
nLx[18,4] = nLx[19,3]/(nLx[18,3]+nLx[19,3])
nLx[20:36,4] = nLx[21:37,3]/nLx[20:36,3]
nLx[37,4] = nLx[38,3]/(nLx[37,3]+nLx[38,3])
colnames(nLx) = c('sexo', 'idade', 'nLx', 'nSx')

pop$nSx = nLx$nSx
pop[2:19,5] = round(pop[1:18,3]*pop[1:18,4],0)
pop[19,5] = round((pop[19,3]+pop[18,3])*pop[18,4],0)
pop[21:38,5] = round(pop[20:37,3]*pop[20:37,4],0)
pop[38,5] = round((pop[38,3]+pop[37,3])*pop[37,4],0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada')

pop$TLM <- 0
pop$pop2015aberta = round(pop$pop2015fechada+(pop$pop2015fechada*pop$TLM),0)

pop$TEF <- NA
pop[4:10,8] = TEF$TEF
pop[1,9] = round(2.5*sum((pop[4:10,3]+pop[4:10,7])*pop[4:10,8]),0)
pop$V9 <- NULL

pop$'9' <- NA
pop$'10' <- NA

pop[1,10] = round(pop[1,9]*(1/(1+1.04))*(nLx[1,3]/500000),0)
pop[20,10] = round(pop[1,9]*(1.04/(1+1.04))*(nLx[20,3]/500000),0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada', 'TLM',
                  'pop2015aberta','TEF', 'B.total', 'B.sexo.sobrev')

pop$pop2040 = round(ifelse(is.na(pop$pop2015aberta), pop$B.sexo.sobrev,
                           pop$pop2015aberta),0)

a2040 <- pop$pop2040

pop <- pop %>%
  select ( sexo, idade, pop2040)

pop$sexo = as.character(pop$sexo)
pop$idade = as.integer(pop$idade)
pop$pop2010 = as.numeric(pop$pop2010)

nLx$sexo = as.character(nLx$sexo)
nLx$idade = as.integer(nLx$idade)
nLx$nLx = as.numeric(nLx$nLx)

TEF$ano = as.numeric(TEF$ano)
TEF$idade = as.integer(TEF$idade)
TEF$TEF = as.numeric(TEF$TEF)

nLx[1:17,4] = nLx[2:18,3]/nLx[1:17,3]
nLx[18,4] = nLx[19,3]/(nLx[18,3]+nLx[19,3])
nLx[20:36,4] = nLx[21:37,3]/nLx[20:36,3]
nLx[37,4] = nLx[38,3]/(nLx[37,3]+nLx[38,3])
colnames(nLx) = c('sexo', 'idade', 'nLx', 'nSx')

pop$nSx = nLx$nSx
pop[2:19,5] = round(pop[1:18,3]*pop[1:18,4],0)
pop[19,5] = round((pop[19,3]+pop[18,3])*pop[18,4],0)
pop[21:38,5] = round(pop[20:37,3]*pop[20:37,4],0)
pop[38,5] = round((pop[38,3]+pop[37,3])*pop[37,4],0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada')

pop$TLM <- 0
pop$pop2015aberta = round(pop$pop2015fechada+(pop$pop2015fechada*pop$TLM),0)

pop$TEF <- NA
pop[4:10,8] = TEF$TEF
pop[1,9] = round(2.5*sum((pop[4:10,3]+pop[4:10,7])*pop[4:10,8]),0)
pop$V9 <- NULL

pop$'9' <- NA
pop$'10' <- NA

pop[1,10] = round(pop[1,9]*(1/(1+1.04))*(nLx[1,3]/500000),0)
pop[20,10] = round(pop[1,9]*(1.04/(1+1.04))*(nLx[20,3]/500000),0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada', 'TLM',
                  'pop2015aberta','TEF', 'B.total', 'B.sexo.sobrev')

pop$pop2045 = round(ifelse(is.na(pop$pop2015aberta), pop$B.sexo.sobrev,
                           pop$pop2015aberta),0)

a2045 <- pop$pop2045

pop <- pop %>%
  select ( sexo, idade, pop2045)

pop$sexo = as.character(pop$sexo)
pop$idade = as.integer(pop$idade)
pop$pop2010 = as.numeric(pop$pop2010)

nLx$sexo = as.character(nLx$sexo)
nLx$idade = as.integer(nLx$idade)
nLx$nLx = as.numeric(nLx$nLx)

TEF$ano = as.numeric(TEF$ano)
TEF$idade = as.integer(TEF$idade)
TEF$TEF = as.numeric(TEF$TEF)

nLx[1:17,4] = nLx[2:18,3]/nLx[1:17,3]
nLx[18,4] = nLx[19,3]/(nLx[18,3]+nLx[19,3])
nLx[20:36,4] = nLx[21:37,3]/nLx[20:36,3]
nLx[37,4] = nLx[38,3]/(nLx[37,3]+nLx[38,3])
colnames(nLx) = c('sexo', 'idade', 'nLx', 'nSx')

pop$nSx = nLx$nSx
pop[2:19,5] = round(pop[1:18,3]*pop[1:18,4],0)
pop[19,5] = round((pop[19,3]+pop[18,3])*pop[18,4],0)
pop[21:38,5] = round(pop[20:37,3]*pop[20:37,4],0)
pop[38,5] = round((pop[38,3]+pop[37,3])*pop[37,4],0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada')

pop$TLM <- 0
pop$pop2015aberta = round(pop$pop2015fechada+(pop$pop2015fechada*pop$TLM),0)

pop$TEF <- NA
pop[4:10,8] = TEF$TEF
pop[1,9] = round(2.5*sum((pop[4:10,3]+pop[4:10,7])*pop[4:10,8]),0)
pop$V9 <- NULL

pop$'9' <- NA
pop$'10' <- NA

pop[1,10] = round(pop[1,9]*(1/(1+1.04))*(nLx[1,3]/500000),0)
pop[20,10] = round(pop[1,9]*(1.04/(1+1.04))*(nLx[20,3]/500000),0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada', 'TLM',
                  'pop2015aberta','TEF', 'B.total', 'B.sexo.sobrev')

pop$pop2050 = round(ifelse(is.na(pop$pop2015aberta), pop$B.sexo.sobrev,
                           pop$pop2015aberta),0)

a2050 <- pop$pop2050

pop <- pop %>%
  select ( sexo, idade, pop2050)

pop$sexo = as.character(pop$sexo)
pop$idade = as.integer(pop$idade)
pop$pop2010 = as.numeric(pop$pop2010)

nLx$sexo = as.character(nLx$sexo)
nLx$idade = as.integer(nLx$idade)
nLx$nLx = as.numeric(nLx$nLx)

TEF$ano = as.numeric(TEF$ano)
TEF$idade = as.integer(TEF$idade)
TEF$TEF = as.numeric(TEF$TEF)

nLx[1:17,4] = nLx[2:18,3]/nLx[1:17,3]
nLx[18,4] = nLx[19,3]/(nLx[18,3]+nLx[19,3])
nLx[20:36,4] = nLx[21:37,3]/nLx[20:36,3]
nLx[37,4] = nLx[38,3]/(nLx[37,3]+nLx[38,3])
colnames(nLx) = c('sexo', 'idade', 'nLx', 'nSx')

pop$nSx = nLx$nSx
pop[2:19,5] = round(pop[1:18,3]*pop[1:18,4],0)
pop[19,5] = round((pop[19,3]+pop[18,3])*pop[18,4],0)
pop[21:38,5] = round(pop[20:37,3]*pop[20:37,4],0)
pop[38,5] = round((pop[38,3]+pop[37,3])*pop[37,4],0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada')

pop$TLM <- 0
pop$pop2015aberta = round(pop$pop2015fechada+(pop$pop2015fechada*pop$TLM),0)

pop$TEF <- NA
pop[4:10,8] = TEF$TEF
pop[1,9] = round(2.5*sum((pop[4:10,3]+pop[4:10,7])*pop[4:10,8]),0)
pop$V9 <- NULL

pop$'9' <- NA
pop$'10' <- NA

pop[1,10] = round(pop[1,9]*(1/(1+1.04))*(nLx[1,3]/500000),0)
pop[20,10] = round(pop[1,9]*(1.04/(1+1.04))*(nLx[20,3]/500000),0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada', 'TLM',
                  'pop2015aberta','TEF', 'B.total', 'B.sexo.sobrev')

pop$pop2055 = round(ifelse(is.na(pop$pop2015aberta), pop$B.sexo.sobrev,
                           pop$pop2015aberta),0)

a2055 <- pop$pop2055

pop <- pop %>%
  select ( sexo, idade, pop2055)

pop$sexo = as.character(pop$sexo)
pop$idade = as.integer(pop$idade)
pop$pop2010 = as.numeric(pop$pop2010)

nLx$sexo = as.character(nLx$sexo)
nLx$idade = as.integer(nLx$idade)
nLx$nLx = as.numeric(nLx$nLx)

TEF$ano = as.numeric(TEF$ano)
TEF$idade = as.integer(TEF$idade)
TEF$TEF = as.numeric(TEF$TEF)

nLx[1:17,4] = nLx[2:18,3]/nLx[1:17,3]
nLx[18,4] = nLx[19,3]/(nLx[18,3]+nLx[19,3])
nLx[20:36,4] = nLx[21:37,3]/nLx[20:36,3]
nLx[37,4] = nLx[38,3]/(nLx[37,3]+nLx[38,3])
colnames(nLx) = c('sexo', 'idade', 'nLx', 'nSx')

pop$nSx = nLx$nSx
pop[2:19,5] = round(pop[1:18,3]*pop[1:18,4],0)
pop[19,5] = round((pop[19,3]+pop[18,3])*pop[18,4],0)
pop[21:38,5] = round(pop[20:37,3]*pop[20:37,4],0)
pop[38,5] = round((pop[38,3]+pop[37,3])*pop[37,4],0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada')

pop$TLM <- 0
pop$pop2015aberta = round(pop$pop2015fechada+(pop$pop2015fechada*pop$TLM),0)

pop$TEF <- NA
pop[4:10,8] = TEF$TEF
pop[1,9] = round(2.5*sum((pop[4:10,3]+pop[4:10,7])*pop[4:10,8]),0)
pop$V9 <- NULL

pop$'9' <- NA
pop$'10' <- NA

pop[1,10] = round(pop[1,9]*(1/(1+1.04))*(nLx[1,3]/500000),0)
pop[20,10] = round(pop[1,9]*(1.04/(1+1.04))*(nLx[20,3]/500000),0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada', 'TLM',
                  'pop2015aberta','TEF', 'B.total', 'B.sexo.sobrev')

pop$pop2060 = round(ifelse(is.na(pop$pop2015aberta), pop$B.sexo.sobrev,
                           pop$pop2015aberta),0)

a2060 <- pop$pop2060

pop <- pop %>%
  select ( sexo, idade, pop2060)

pop$sexo = as.character(pop$sexo)
pop$idade = as.integer(pop$idade)
pop$pop2010 = as.numeric(pop$pop2010)

nLx$sexo = as.character(nLx$sexo)
nLx$idade = as.integer(nLx$idade)
nLx$nLx = as.numeric(nLx$nLx)

TEF$ano = as.numeric(TEF$ano)
TEF$idade = as.integer(TEF$idade)
TEF$TEF = as.numeric(TEF$TEF)

nLx[1:17,4] = nLx[2:18,3]/nLx[1:17,3]
nLx[18,4] = nLx[19,3]/(nLx[18,3]+nLx[19,3])
nLx[20:36,4] = nLx[21:37,3]/nLx[20:36,3]
nLx[37,4] = nLx[38,3]/(nLx[37,3]+nLx[38,3])
colnames(nLx) = c('sexo', 'idade', 'nLx', 'nSx')

pop$nSx = nLx$nSx
pop[2:19,5] = round(pop[1:18,3]*pop[1:18,4],0)
pop[19,5] = round((pop[19,3]+pop[18,3])*pop[18,4],0)
pop[21:38,5] = round(pop[20:37,3]*pop[20:37,4],0)
pop[38,5] = round((pop[38,3]+pop[37,3])*pop[37,4],0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada')

pop$TLM <- 0
pop$pop2015aberta = round(pop$pop2015fechada+(pop$pop2015fechada*pop$TLM),0)

pop$TEF <- NA
pop[4:10,8] = TEF$TEF
pop[1,9] = round(2.5*sum((pop[4:10,3]+pop[4:10,7])*pop[4:10,8]),0)
pop$V9 <- NULL

pop$'9' <- NA
pop$'10' <- NA

pop[1,10] = round(pop[1,9]*(1/(1+1.04))*(nLx[1,3]/500000),0)
pop[20,10] = round(pop[1,9]*(1.04/(1+1.04))*(nLx[20,3]/500000),0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada', 'TLM',
                  'pop2015aberta','TEF', 'B.total', 'B.sexo.sobrev')

pop$pop2065 = round(ifelse(is.na(pop$pop2015aberta), pop$B.sexo.sobrev,
                           pop$pop2015aberta),0)

a2065 <- pop$pop2065

pop <- pop %>%
  select ( sexo, idade, pop2065)

pop$sexo = as.character(pop$sexo)
pop$idade = as.integer(pop$idade)
pop$pop2010 = as.numeric(pop$pop2010)

nLx$sexo = as.character(nLx$sexo)
nLx$idade = as.integer(nLx$idade)
nLx$nLx = as.numeric(nLx$nLx)

TEF$ano = as.numeric(TEF$ano)
TEF$idade = as.integer(TEF$idade)
TEF$TEF = as.numeric(TEF$TEF)

nLx[1:17,4] = nLx[2:18,3]/nLx[1:17,3]
nLx[18,4] = nLx[19,3]/(nLx[18,3]+nLx[19,3])
nLx[20:36,4] = nLx[21:37,3]/nLx[20:36,3]
nLx[37,4] = nLx[38,3]/(nLx[37,3]+nLx[38,3])
colnames(nLx) = c('sexo', 'idade', 'nLx', 'nSx')

pop$nSx = nLx$nSx
pop[2:19,5] = round(pop[1:18,3]*pop[1:18,4],0)
pop[19,5] = round((pop[19,3]+pop[18,3])*pop[18,4],0)
pop[21:38,5] = round(pop[20:37,3]*pop[20:37,4],0)
pop[38,5] = round((pop[38,3]+pop[37,3])*pop[37,4],0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada')

pop$TLM <- 0
pop$pop2015aberta = round(pop$pop2015fechada+(pop$pop2015fechada*pop$TLM),0)

pop$TEF <- NA
pop[4:10,8] = TEF$TEF
pop[1,9] = round(2.5*sum((pop[4:10,3]+pop[4:10,7])*pop[4:10,8]),0)
pop$V9 <- NULL

pop$'9' <- NA
pop$'10' <- NA

pop[1,10] = round(pop[1,9]*(1/(1+1.04))*(nLx[1,3]/500000),0)
pop[20,10] = round(pop[1,9]*(1.04/(1+1.04))*(nLx[20,3]/500000),0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada', 'TLM',
                  'pop2015aberta','TEF', 'B.total', 'B.sexo.sobrev')

pop$pop2070 = round(ifelse(is.na(pop$pop2015aberta), pop$B.sexo.sobrev,
                           pop$pop2015aberta),0)

a2070 <- pop$pop2070

pop <- pop %>%
  select ( sexo, idade, pop2070)

pop$sexo = as.character(pop$sexo)
pop$idade = as.integer(pop$idade)
pop$pop2010 = as.numeric(pop$pop2010)

nLx$sexo = as.character(nLx$sexo)
nLx$idade = as.integer(nLx$idade)
nLx$nLx = as.numeric(nLx$nLx)

TEF$ano = as.numeric(TEF$ano)
TEF$idade = as.integer(TEF$idade)
TEF$TEF = as.numeric(TEF$TEF)

nLx[1:17,4] = nLx[2:18,3]/nLx[1:17,3]
nLx[18,4] = nLx[19,3]/(nLx[18,3]+nLx[19,3])
nLx[20:36,4] = nLx[21:37,3]/nLx[20:36,3]
nLx[37,4] = nLx[38,3]/(nLx[37,3]+nLx[38,3])
colnames(nLx) = c('sexo', 'idade', 'nLx', 'nSx')

pop$nSx = nLx$nSx
pop[2:19,5] = round(pop[1:18,3]*pop[1:18,4],0)
pop[19,5] = round((pop[19,3]+pop[18,3])*pop[18,4],0)
pop[21:38,5] = round(pop[20:37,3]*pop[20:37,4],0)
pop[38,5] = round((pop[38,3]+pop[37,3])*pop[37,4],0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada')

pop$TLM <- 0
pop$pop2015aberta = round(pop$pop2015fechada+(pop$pop2015fechada*pop$TLM),0)

pop$TEF <- NA
pop[4:10,8] = TEF$TEF
pop[1,9] = round(2.5*sum((pop[4:10,3]+pop[4:10,7])*pop[4:10,8]),0)
pop$V9 <- NULL

pop$'9' <- NA
pop$'10' <- NA

pop[1,10] = round(pop[1,9]*(1/(1+1.04))*(nLx[1,3]/500000),0)
pop[20,10] = round(pop[1,9]*(1.04/(1+1.04))*(nLx[20,3]/500000),0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada', 'TLM',
                  'pop2015aberta','TEF', 'B.total', 'B.sexo.sobrev')

pop$pop2075 = round(ifelse(is.na(pop$pop2015aberta), pop$B.sexo.sobrev,
                           pop$pop2015aberta),0)

a2075 <- pop$pop2075

pop <- pop %>%
  select ( sexo, idade, pop2075)

pop$sexo = as.character(pop$sexo)
pop$idade = as.integer(pop$idade)
pop$pop2010 = as.numeric(pop$pop2010)

nLx$sexo = as.character(nLx$sexo)
nLx$idade = as.integer(nLx$idade)
nLx$nLx = as.numeric(nLx$nLx)

TEF$ano = as.numeric(TEF$ano)
TEF$idade = as.integer(TEF$idade)
TEF$TEF = as.numeric(TEF$TEF)

nLx[1:17,4] = nLx[2:18,3]/nLx[1:17,3]
nLx[18,4] = nLx[19,3]/(nLx[18,3]+nLx[19,3])
nLx[20:36,4] = nLx[21:37,3]/nLx[20:36,3]
nLx[37,4] = nLx[38,3]/(nLx[37,3]+nLx[38,3])
colnames(nLx) = c('sexo', 'idade', 'nLx', 'nSx')

pop$nSx = nLx$nSx
pop[2:19,5] = round(pop[1:18,3]*pop[1:18,4],0)
pop[19,5] = round((pop[19,3]+pop[18,3])*pop[18,4],0)
pop[21:38,5] = round(pop[20:37,3]*pop[20:37,4],0)
pop[38,5] = round((pop[38,3]+pop[37,3])*pop[37,4],0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada')

pop$TLM <- 0
pop$pop2015aberta = round(pop$pop2015fechada+(pop$pop2015fechada*pop$TLM),0)

pop$TEF <- NA
pop[4:10,8] = TEF$TEF
pop[1,9] = round(2.5*sum((pop[4:10,3]+pop[4:10,7])*pop[4:10,8]),0)
pop$V9 <- NULL

pop$'9' <- NA
pop$'10' <- NA

pop[1,10] = round(pop[1,9]*(1/(1+1.04))*(nLx[1,3]/500000),0)
pop[20,10] = round(pop[1,9]*(1.04/(1+1.04))*(nLx[20,3]/500000),0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada', 'TLM',
                  'pop2015aberta','TEF', 'B.total', 'B.sexo.sobrev')

pop$pop2080 = round(ifelse(is.na(pop$pop2015aberta), pop$B.sexo.sobrev,
                           pop$pop2015aberta),0)

a2080 <- pop$pop2080

pop <- pop %>%
  select ( sexo, idade, pop2080)

pop$sexo = as.character(pop$sexo)
pop$idade = as.integer(pop$idade)
pop$pop2010 = as.numeric(pop$pop2010)

nLx$sexo = as.character(nLx$sexo)
nLx$idade = as.integer(nLx$idade)
nLx$nLx = as.numeric(nLx$nLx)

TEF$ano = as.numeric(TEF$ano)
TEF$idade = as.integer(TEF$idade)
TEF$TEF = as.numeric(TEF$TEF)

nLx[1:17,4] = nLx[2:18,3]/nLx[1:17,3]
nLx[18,4] = nLx[19,3]/(nLx[18,3]+nLx[19,3])
nLx[20:36,4] = nLx[21:37,3]/nLx[20:36,3]
nLx[37,4] = nLx[38,3]/(nLx[37,3]+nLx[38,3])
colnames(nLx) = c('sexo', 'idade', 'nLx', 'nSx')

pop$nSx = nLx$nSx
pop[2:19,5] = round(pop[1:18,3]*pop[1:18,4],0)
pop[19,5] = round((pop[19,3]+pop[18,3])*pop[18,4],0)
pop[21:38,5] = round(pop[20:37,3]*pop[20:37,4],0)
pop[38,5] = round((pop[38,3]+pop[37,3])*pop[37,4],0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada')

pop$TLM <- 0
pop$pop2015aberta = round(pop$pop2015fechada+(pop$pop2015fechada*pop$TLM),0)

pop$TEF <- NA
pop[4:10,8] = TEF$TEF
pop[1,9] = round(2.5*sum((pop[4:10,3]+pop[4:10,7])*pop[4:10,8]),0)
pop$V9 <- NULL

pop$'9' <- NA
pop$'10' <- NA

pop[1,10] = round(pop[1,9]*(1/(1+1.04))*(nLx[1,3]/500000),0)
pop[20,10] = round(pop[1,9]*(1.04/(1+1.04))*(nLx[20,3]/500000),0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada', 'TLM',
                  'pop2015aberta','TEF', 'B.total', 'B.sexo.sobrev')

pop$pop2085 = round(ifelse(is.na(pop$pop2015aberta), pop$B.sexo.sobrev,
                           pop$pop2015aberta),0)

a2085 <- pop$pop2085

pop <- pop %>%
  select ( sexo, idade, pop2085)

pop$sexo = as.character(pop$sexo)
pop$idade = as.integer(pop$idade)
pop$pop2010 = as.numeric(pop$pop2010)

nLx$sexo = as.character(nLx$sexo)
nLx$idade = as.integer(nLx$idade)
nLx$nLx = as.numeric(nLx$nLx)

TEF$ano = as.numeric(TEF$ano)
TEF$idade = as.integer(TEF$idade)
TEF$TEF = as.numeric(TEF$TEF)

nLx[1:17,4] = nLx[2:18,3]/nLx[1:17,3]
nLx[18,4] = nLx[19,3]/(nLx[18,3]+nLx[19,3])
nLx[20:36,4] = nLx[21:37,3]/nLx[20:36,3]
nLx[37,4] = nLx[38,3]/(nLx[37,3]+nLx[38,3])
colnames(nLx) = c('sexo', 'idade', 'nLx', 'nSx')

pop$nSx = nLx$nSx
pop[2:19,5] = round(pop[1:18,3]*pop[1:18,4],0)
pop[19,5] = round((pop[19,3]+pop[18,3])*pop[18,4],0)
pop[21:38,5] = round(pop[20:37,3]*pop[20:37,4],0)
pop[38,5] = round((pop[38,3]+pop[37,3])*pop[37,4],0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada')

pop$TLM <- 0
pop$pop2015aberta = round(pop$pop2015fechada+(pop$pop2015fechada*pop$TLM),0)

pop$TEF <- NA
pop[4:10,8] = TEF$TEF
pop[1,9] = round(2.5*sum((pop[4:10,3]+pop[4:10,7])*pop[4:10,8]),0)
pop$V9 <- NULL

pop$'9' <- NA
pop$'10' <- NA

pop[1,10] = round(pop[1,9]*(1/(1+1.04))*(nLx[1,3]/500000),0)
pop[20,10] = round(pop[1,9]*(1.04/(1+1.04))*(nLx[20,3]/500000),0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada', 'TLM',
                  'pop2015aberta','TEF', 'B.total', 'B.sexo.sobrev')

pop$pop2090 = round(ifelse(is.na(pop$pop2015aberta), pop$B.sexo.sobrev,
                           pop$pop2015aberta),0)

a2090 <- pop$pop2090

pop <- pop %>%
  select ( sexo, idade, pop2090)

pop$sexo = as.character(pop$sexo)
pop$idade = as.integer(pop$idade)
pop$pop2010 = as.numeric(pop$pop2010)

nLx$sexo = as.character(nLx$sexo)
nLx$idade = as.integer(nLx$idade)
nLx$nLx = as.numeric(nLx$nLx)

TEF$ano = as.numeric(TEF$ano)
TEF$idade = as.integer(TEF$idade)
TEF$TEF = as.numeric(TEF$TEF)

nLx[1:17,4] = nLx[2:18,3]/nLx[1:17,3]
nLx[18,4] = nLx[19,3]/(nLx[18,3]+nLx[19,3])
nLx[20:36,4] = nLx[21:37,3]/nLx[20:36,3]
nLx[37,4] = nLx[38,3]/(nLx[37,3]+nLx[38,3])
colnames(nLx) = c('sexo', 'idade', 'nLx', 'nSx')

pop$nSx = nLx$nSx
pop[2:19,5] = round(pop[1:18,3]*pop[1:18,4],0)
pop[19,5] = round((pop[19,3]+pop[18,3])*pop[18,4],0)
pop[21:38,5] = round(pop[20:37,3]*pop[20:37,4],0)
pop[38,5] = round((pop[38,3]+pop[37,3])*pop[37,4],0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada')

pop$TLM <- 0
pop$pop2015aberta = round(pop$pop2015fechada+(pop$pop2015fechada*pop$TLM),0)

pop$TEF <- NA
pop[4:10,8] = TEF$TEF
pop[1,9] = round(2.5*sum((pop[4:10,3]+pop[4:10,7])*pop[4:10,8]),0)
pop$V9 <- NULL

pop$'9' <- NA
pop$'10' <- NA

pop[1,10] = round(pop[1,9]*(1/(1+1.04))*(nLx[1,3]/500000),0)
pop[20,10] = round(pop[1,9]*(1.04/(1+1.04))*(nLx[20,3]/500000),0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada', 'TLM',
                  'pop2015aberta','TEF', 'B.total', 'B.sexo.sobrev')

pop$pop2095 = round(ifelse(is.na(pop$pop2015aberta), pop$B.sexo.sobrev,
                           pop$pop2015aberta),0)

a2095 <- pop$pop2095

pop <- pop %>%
  select ( sexo, idade, pop2095)

pop$sexo = as.character(pop$sexo)
pop$idade = as.integer(pop$idade)
pop$pop2010 = as.numeric(pop$pop2010)

nLx$sexo = as.character(nLx$sexo)
nLx$idade = as.integer(nLx$idade)
nLx$nLx = as.numeric(nLx$nLx)

TEF$ano = as.numeric(TEF$ano)
TEF$idade = as.integer(TEF$idade)
TEF$TEF = as.numeric(TEF$TEF)

nLx[1:17,4] = nLx[2:18,3]/nLx[1:17,3]
nLx[18,4] = nLx[19,3]/(nLx[18,3]+nLx[19,3])
nLx[20:36,4] = nLx[21:37,3]/nLx[20:36,3]
nLx[37,4] = nLx[38,3]/(nLx[37,3]+nLx[38,3])
colnames(nLx) = c('sexo', 'idade', 'nLx', 'nSx')

pop$nSx = nLx$nSx
pop[2:19,5] = round(pop[1:18,3]*pop[1:18,4],0)
pop[19,5] = round((pop[19,3]+pop[18,3])*pop[18,4],0)
pop[21:38,5] = round(pop[20:37,3]*pop[20:37,4],0)
pop[38,5] = round((pop[38,3]+pop[37,3])*pop[37,4],0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada')

pop$TLM <- 0
pop$pop2015aberta = round(pop$pop2015fechada+(pop$pop2015fechada*pop$TLM),0)

pop$TEF <- NA
pop[4:10,8] = TEF$TEF
pop[1,9] = round(2.5*sum((pop[4:10,3]+pop[4:10,7])*pop[4:10,8]),0)
pop$V9 <- NULL

pop$'9' <- NA
pop$'10' <- NA

pop[1,10] = round(pop[1,9]*(1/(1+1.04))*(nLx[1,3]/500000),0)
pop[20,10] = round(pop[1,9]*(1.04/(1+1.04))*(nLx[20,3]/500000),0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada', 'TLM',
                  'pop2015aberta','TEF', 'B.total', 'B.sexo.sobrev')

pop$pop2100 = round(ifelse(is.na(pop$pop2015aberta), pop$B.sexo.sobrev,
                           pop$pop2015aberta),0)

a2100 <- pop$pop2100

pop <- pop %>%
  select ( sexo, idade, pop2100)

pop$sexo = as.character(pop$sexo)
pop$idade = as.integer(pop$idade)
pop$pop2010 = as.numeric(pop$pop2010)

nLx$sexo = as.character(nLx$sexo)
nLx$idade = as.integer(nLx$idade)
nLx$nLx = as.numeric(nLx$nLx)

TEF$ano = as.numeric(TEF$ano)
TEF$idade = as.integer(TEF$idade)
TEF$TEF = as.numeric(TEF$TEF)

nLx[1:17,4] = nLx[2:18,3]/nLx[1:17,3]
nLx[18,4] = nLx[19,3]/(nLx[18,3]+nLx[19,3])
nLx[20:36,4] = nLx[21:37,3]/nLx[20:36,3]
nLx[37,4] = nLx[38,3]/(nLx[37,3]+nLx[38,3])
colnames(nLx) = c('sexo', 'idade', 'nLx', 'nSx')

pop$nSx = nLx$nSx
pop[2:19,5] = round(pop[1:18,3]*pop[1:18,4],0)
pop[19,5] = round((pop[19,3]+pop[18,3])*pop[18,4],0)
pop[21:38,5] = round(pop[20:37,3]*pop[20:37,4],0)
pop[38,5] = round((pop[38,3]+pop[37,3])*pop[37,4],0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada')

pop$TLM <- 0
pop$pop2015aberta = round(pop$pop2015fechada+(pop$pop2015fechada*pop$TLM),0)

pop$TEF <- NA
pop[4:10,8] = TEF$TEF
pop[1,9] = round(2.5*sum((pop[4:10,3]+pop[4:10,7])*pop[4:10,8]),0)
pop$V9 <- NULL

pop$'9' <- NA
pop$'10' <- NA

pop[1,10] = round(pop[1,9]*(1/(1+1.04))*(nLx[1,3]/500000),0)
pop[20,10] = round(pop[1,9]*(1.04/(1+1.04))*(nLx[20,3]/500000),0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada', 'TLM',
                  'pop2015aberta','TEF', 'B.total', 'B.sexo.sobrev')

pop$pop2105 = round(ifelse(is.na(pop$pop2015aberta), pop$B.sexo.sobrev,
                           pop$pop2015aberta),0)

a2105 <- pop$pop2105

pop <- pop %>%
  select ( sexo, idade, pop2105)

pop$sexo = as.character(pop$sexo)
pop$idade = as.integer(pop$idade)
pop$pop2010 = as.numeric(pop$pop2010)

nLx$sexo = as.character(nLx$sexo)
nLx$idade = as.integer(nLx$idade)
nLx$nLx = as.numeric(nLx$nLx)

TEF$ano = as.numeric(TEF$ano)
TEF$idade = as.integer(TEF$idade)
TEF$TEF = as.numeric(TEF$TEF)

nLx[1:17,4] = nLx[2:18,3]/nLx[1:17,3]
nLx[18,4] = nLx[19,3]/(nLx[18,3]+nLx[19,3])
nLx[20:36,4] = nLx[21:37,3]/nLx[20:36,3]
nLx[37,4] = nLx[38,3]/(nLx[37,3]+nLx[38,3])
colnames(nLx) = c('sexo', 'idade', 'nLx', 'nSx')

pop$nSx = nLx$nSx
pop[2:19,5] = round(pop[1:18,3]*pop[1:18,4],0)
pop[19,5] = round((pop[19,3]+pop[18,3])*pop[18,4],0)
pop[21:38,5] = round(pop[20:37,3]*pop[20:37,4],0)
pop[38,5] = round((pop[38,3]+pop[37,3])*pop[37,4],0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada')

pop$TLM <- 0
pop$pop2015aberta = round(pop$pop2015fechada+(pop$pop2015fechada*pop$TLM),0)

pop$TEF <- NA
pop[4:10,8] = TEF$TEF
pop[1,9] = round(2.5*sum((pop[4:10,3]+pop[4:10,7])*pop[4:10,8]),0)
pop$V9 <- NULL

pop$'9' <- NA
pop$'10' <- NA

pop[1,10] = round(pop[1,9]*(1/(1+1.04))*(nLx[1,3]/500000),0)
pop[20,10] = round(pop[1,9]*(1.04/(1+1.04))*(nLx[20,3]/500000),0)
colnames(pop) = c('sexo', 'idade', 'pop2010', 'nSx', 'pop2015fechada', 'TLM',
                  'pop2015aberta','TEF', 'B.total', 'B.sexo.sobrev')

pop$pop2110 = round(ifelse(is.na(pop$pop2015aberta), pop$B.sexo.sobrev,
                           pop$pop2015aberta),0)

a2110 <- pop$pop2110

proj <- data.frame(a2010, a2015,a2020,a2025,a2030,a2035,
                   a2040,a2045,a2050,a2055,a2060,a2065,a2070,a2075,
                   a2080,a2085,a2090,a2095,a2100,a2105,a2110) 

write.csv(proj,'projPE.csv', row.names = FALSE)