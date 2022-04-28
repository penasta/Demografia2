library(lubridate)
library(magrittr)
library(dplyr)

# Carregar as planilhas DOPA.csv e DOPE.csv

###

pa20 <- filter(DOPA, ANO == "2020")
pa21 <- filter(DOPA, ANO == "2021")


pe20 <- filter(DOPE, ANO == "2020")
pe21 <- filter(DOPE, ANO == "2021")

####

pa20$DTOBITO2 <- NA
pa20$DTOBITO2 <- dmy(pa20$DTOBITO)
pa21$DTOBITO2 <- NA
pa21$DTOBITO2 <- dmy(pa21$DTOBITO)

pe20$DTOBITO2 <- NA
pe20$DTOBITO2 <- dmy(pe20$DTOBITO)
pe21$DTOBITO2 <- NA
pe21$DTOBITO2 <- dmy(pe21$DTOBITO)



vinte <- ("2020-01-01")
vinteum <- ("2021-01-01")



pa20$DIF <- NA
pa20$DIF <- difftime(pa20$DTOBITO2,vinte, units="days")/365.25
pa20$DIF<-as.numeric(pa20$DIF)
is.numeric(pa20$DIF)

pa21$DIF <- NA
pa21$DIF <- difftime(pa21$DTOBITO2,vinteum, units="days")/365.25
pa21$DIF<-as.numeric(pa21$DIF)
is.numeric(pa21$DIF)


pe20$DIF <- NA
pe20$DIF <- difftime(pe20$DTOBITO2,vinte, units="days")/365.25
pe20$DIF<-as.numeric(pe20$DIF)
is.numeric(pe20$DIF)

pe21$DIF <- NA
pe21$DIF <- difftime(pe21$DTOBITO2,vinteum, units="days")/365.25
pe21$DIF<-as.numeric(pe21$DIF)
is.numeric(pe21$DIF)

####

DOPA2 <- rbind(pa20,pa21)

DOPE2 <- rbind(pe20,pe21)

####

rm(DOPA)
rm(DOPE)
rm(pa20)
rm(pa21)
rm(pe20)
rm(pe21)

####

write.csv(DOPA2,"DOPA2.csv")
write.csv(DOPE2,"DOPE2.csv")

####

##############################################################################

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

##############################################################################

# Falta somar e tirar a média de DIF por IDADE SIMPLES
# Após isso, agregar pelos grupos quinquenais de idade. Está pronto o nKx.

#----------------------------------------------------------

write.csv(DOPA,"DOPA2.csv")
write.csv(DOPE,"DOPE2.csv")