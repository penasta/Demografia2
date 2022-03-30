setwd ("./projeção")

#Pará

pop = read.csv('pop2010.PA.csv', sep=',', stringsAsFactors = FALSE)
nLx = read.csv('nLxPA.csv', sep=',', stringsAsFactors = FALSE)
TEF = read.csv('TEF.PA.csv', sep=',', stringsAsFactors = FALSE)

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

pop$pop2015 = round(ifelse(is.na(pop$pop2015aberta), pop$B.sexo.sobrev,
                           pop$pop2015aberta),0)


write.csv(pop,'tabuaPA.csv', row.names = FALSE)


#Pernambuco

pop = read.csv('pop2010.PE.csv', sep=',', stringsAsFactors = FALSE)
nLx = read.csv('nLxPE.csv', sep=',', stringsAsFactors = FALSE)
TEF = read.csv('TEF.PE.csv', sep=',', stringsAsFactors = FALSE)

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

pop$pop2015 = round(ifelse(is.na(pop$pop2015aberta), pop$B.sexo.sobrev,
                           pop$pop2015aberta),0)

write.csv(pop,'tabuaPE.csv', row.names = FALSE)