library(lubridate)

# Carregar as planilhas DOPA.csv e DOPE.csv

DOPA$DTOBITO2 <- NA
DOPA$DTOBITO2 <- dmy(DOPA$DTOBITO)

DOPE$DTOBITO2 <- NA
DOPE$DTOBITO2 <- dmy(DOPE$DTOBITO)

DOPA$DIF <- NA
DOPE$DIF <- NA

um <- ("2019-01-01")

DOPA$DIF <- difftime(DOPA$DTOBITO2,um, units="days")/365.25
DOPA$DIF<-as.numeric(DOPA$DIF)
is.numeric(DOPA$DIF)

DOPE$DIF <- difftime(DOPE$DTOBITO2,um, units="days")/365.25
DOPE$DIF<-as.numeric(DOPE$DIF)
is.numeric(DOPE$DIF)

# Falta arrumar os valores maiores que 1 na variável DOPE$DIF
# Após isso, somar e tirar a média de DIF por IDADE SIMPLES
# Após isso, agregar pelos grupos quinquenais de idade. Está pronto o nKx.

#----------------------------------------------------------

write.csv(DOPA,"DOPA2.csv")
write.csv(DOPE,"DOPE2.csv")