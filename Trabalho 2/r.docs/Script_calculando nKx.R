library(dplyr)

### Carregar DOPA2 e DOPE2

call <- function (grupo,banco,idade){
  grupo <- filter(banco, IDADE == idade)
}

#------------------------------------------------------------------------------

sexo1pa <- DOPA2 %>%
  select(IDADE, SEXO,DIF) %>%
  filter(SEXO == "1") %>%
  select(IDADE,DIF)

zero <- call(um,sexo1pa,"0")
nkx0 <- sum(zero$DIF)/nrow(zero)

uma4 <- call(um,sexo1pa,c("1","2","3","4"))
nkx1a4 <- sum(uma4$DIF)/nrow(uma4)*4

cincoa9 <- call(um,sexo1pa,c("5","6","7","8","9"))
nkx5a9 <- sum(cincoa9$DIF)/nrow(cincoa9)*5

deza14 <- call(um,sexo1pa,c("10","11","12","13","14"))
nkx10a14 <- sum(deza14$DIF)/nrow(deza14)*5

quinzea19 <- call(um,sexo1pa,c("15","16","17","18","19"))
nkx15a19 <- sum(quinzea19$DIF)/nrow(quinzea19)*5

vintea24 <- call(um,sexo1pa,c("20","21","22","23","24"))
nkx20a24 <- sum(vintea24$DIF)/nrow(vintea24)*5

vinte5a29 <- call(um,sexo1pa,c("25","26","27","28","29"))
nkx25a29 <- sum(vinte5a29$DIF)/nrow(vinte5a29)*5

trintaa34 <- call(um,sexo1pa,c("30","31","32","33","34"))
nkx30a34 <- sum(trintaa34$DIF)/nrow(trintaa34)*5

trintae5a39 <- call(um,sexo1pa,c("35","36","37","38","39"))
nkx35a39 <- sum(trintae5a39$DIF)/nrow(trintae5a39)*5

quarentaa44 <- call(um,sexo1pa,c("40","41","42","43","44"))
nkx40a44 <- sum(quarentaa44$DIF)/nrow(quarentaa44)*5

quarentae5a49 <- call(um,sexo1pa,c("45","46","47","48","49"))
nkx45a49 <- sum(quarentae5a49$DIF)/nrow(quarentae5a49)*5

cinquentaa54 <- call(um,sexo1pa,c("50","51","52","53","54"))
nkx50a54 <- sum(cinquentaa54$DIF)/nrow(cinquentaa54)*5

cinquentae5a59 <- call(um,sexo1pa,c("55","56","57","58","59"))
nkx55a59 <- sum(cinquentae5a59$DIF)/nrow(cinquentae5a59)*5

sessentaa64 <- call(um,sexo1pa,c("60","61","62","63","64"))
nkx60a64 <- sum(sessentaa64$DIF)/nrow(sessentaa64)*5

sessentae5a69 <- call(um,sexo1pa,c("65","66","67","68","69"))
nkx65a69 <- sum(sessentae5a69$DIF)/nrow(sessentae5a69)*5

setentaa74 <- call(um,sexo1pa,c("70","71","72","73","74"))
nkx70a74 <- sum(setentaa74$DIF)/nrow(setentaa74)*5

setentae5a79 <- call(um,sexo1pa,c("75","76","77","78","79"))
nkx75a79 <- sum(setentae5a79$DIF)/nrow(setentae5a79)*5

oitentaa84 <- call(um,sexo1pa,c("80","81","82","83","84"))
nkx80a84 <- sum(oitentaa84$DIF)/nrow(oitentaa84)*5

oitentae5a89 <- call(um,sexo1pa,c("85","86","87","88","89"))
nkx85a89 <- sum(oitentae5a89$DIF)/nrow(oitentae5a89)*5


noventamais <- call(um,sexo1pa,c("90","91","92","93","94",
                               "95","96","97","98","99"))
nkx90mais <- sum(noventamais$DIF)/nrow(noventamais)*10


nkxpasexo1 <- c(nkx0,nkx1a4,nkx5a9,nkx10a14,nkx15a19,nkx20a24,nkx25a29,nkx30a34,
                nkx35a39,nkx40a44,nkx45a49,nkx50a54,nkx55a59,nkx60a64,nkx65a69,
                nkx70a74,nkx75a79,nkx80a84,nkx85a89,nkx90mais)
nkxpasexo1 <- as.data.frame(nkxpasexo1)

# write.csv(nkxpasexo1,"nkxpasexo1.csv")


#------------------------------------------------------------------------------

sexo2pa <- DOPA2 %>%
  select(IDADE, SEXO,DIF) %>%
  filter(SEXO == "2") %>%
  select(IDADE,DIF)

zero <- call(um,sexo2pa,"0")
nkx0 <- sum(zero$DIF)/nrow(zero)

uma4 <- call(um,sexo2pa,c("1","2","3","4"))
nkx1a4 <- sum(uma4$DIF)/nrow(uma4)*4

cincoa9 <- call(um,sexo2pa,c("5","6","7","8","9"))
nkx5a9 <- sum(cincoa9$DIF)/nrow(cincoa9)*5

deza14 <- call(um,sexo2pa,c("10","11","12","13","14"))
nkx10a14 <- sum(deza14$DIF)/nrow(deza14)*5

quinzea19 <- call(um,sexo2pa,c("15","16","17","18","19"))
nkx15a19 <- sum(quinzea19$DIF)/nrow(quinzea19)*5

vintea24 <- call(um,sexo2pa,c("20","21","22","23","24"))
nkx20a24 <- sum(vintea24$DIF)/nrow(vintea24)*5

vinte5a29 <- call(um,sexo2pa,c("25","26","27","28","29"))
nkx25a29 <- sum(vinte5a29$DIF)/nrow(vinte5a29)*5

trintaa34 <- call(um,sexo2pa,c("30","31","32","33","34"))
nkx30a34 <- sum(trintaa34$DIF)/nrow(trintaa34)*5

trintae5a39 <- call(um,sexo2pa,c("35","36","37","38","39"))
nkx35a39 <- sum(trintae5a39$DIF)/nrow(trintae5a39)*5

quarentaa44 <- call(um,sexo2pa,c("40","41","42","43","44"))
nkx40a44 <- sum(quarentaa44$DIF)/nrow(quarentaa44)*5

quarentae5a49 <- call(um,sexo2pa,c("45","46","47","48","49"))
nkx45a49 <- sum(quarentae5a49$DIF)/nrow(quarentae5a49)*5

cinquentaa54 <- call(um,sexo2pa,c("50","51","52","53","54"))
nkx50a54 <- sum(cinquentaa54$DIF)/nrow(cinquentaa54)*5

cinquentae5a59 <- call(um,sexo2pa,c("55","56","57","58","59"))
nkx55a59 <- sum(cinquentae5a59$DIF)/nrow(cinquentae5a59)*5

sessentaa64 <- call(um,sexo2pa,c("60","61","62","63","64"))
nkx60a64 <- sum(sessentaa64$DIF)/nrow(sessentaa64)*5

sessentae5a69 <- call(um,sexo2pa,c("65","66","67","68","69"))
nkx65a69 <- sum(sessentae5a69$DIF)/nrow(sessentae5a69)*5

setentaa74 <- call(um,sexo2pa,c("70","71","72","73","74"))
nkx70a74 <- sum(setentaa74$DIF)/nrow(setentaa74)*5

setentae5a79 <- call(um,sexo2pa,c("75","76","77","78","79"))
nkx75a79 <- sum(setentae5a79$DIF)/nrow(setentae5a79)*5

oitentaa84 <- call(um,sexo2pa,c("80","81","82","83","84"))
nkx80a84 <- sum(oitentaa84$DIF)/nrow(oitentaa84)*5

oitentae5a89 <- call(um,sexo2pa,c("85","86","87","88","89"))
nkx85a89 <- sum(oitentae5a89$DIF)/nrow(oitentae5a89)*5


noventamais <- call(um,sexo2pa,c("90","91","92","93","94",
                                 "95","96","97","98","99"))
nkx90mais <- sum(noventamais$DIF)/nrow(noventamais)*10


nkxpasexo2 <- c(nkx0,nkx1a4,nkx5a9,nkx10a14,nkx15a19,nkx20a24,nkx25a29,nkx30a34,
                nkx35a39,nkx40a44,nkx45a49,nkx50a54,nkx55a59,nkx60a64,nkx65a69,
                nkx70a74,nkx75a79,nkx80a84,nkx85a89,nkx90mais)
nkxpasexo2 <- as.data.frame(nkxpasexo2)

# write.csv(nkxpasexo2,"nkxpasexo2.csv")

#------------------------------------------------------------------------------

sexo1pe <- DOPE2 %>%
  select(IDADE, SEXO,DIF) %>%
  filter(SEXO == "1") %>%
  select(IDADE,DIF)

zero <- call(um,sexo1pe,"0")
nkx0 <- sum(zero$DIF)/nrow(zero)

uma4 <- call(um,sexo1pe,c("1","2","3","4"))
nkx1a4 <- sum(uma4$DIF)/nrow(uma4)*4

cincoa9 <- call(um,sexo1pe,c("5","6","7","8","9"))
nkx5a9 <- sum(cincoa9$DIF)/nrow(cincoa9)*5

deza14 <- call(um,sexo1pe,c("10","11","12","13","14"))
nkx10a14 <- sum(deza14$DIF)/nrow(deza14)*5

quinzea19 <- call(um,sexo1pe,c("15","16","17","18","19"))
nkx15a19 <- sum(quinzea19$DIF)/nrow(quinzea19)*5

vintea24 <- call(um,sexo1pe,c("20","21","22","23","24"))
nkx20a24 <- sum(vintea24$DIF)/nrow(vintea24)*5

vinte5a29 <- call(um,sexo1pe,c("25","26","27","28","29"))
nkx25a29 <- sum(vinte5a29$DIF)/nrow(vinte5a29)*5

trintaa34 <- call(um,sexo1pe,c("30","31","32","33","34"))
nkx30a34 <- sum(trintaa34$DIF)/nrow(trintaa34)*5

trintae5a39 <- call(um,sexo1pe,c("35","36","37","38","39"))
nkx35a39 <- sum(trintae5a39$DIF)/nrow(trintae5a39)*5

quarentaa44 <- call(um,sexo1pe,c("40","41","42","43","44"))
nkx40a44 <- sum(quarentaa44$DIF)/nrow(quarentaa44)*5

quarentae5a49 <- call(um,sexo1pe,c("45","46","47","48","49"))
nkx45a49 <- sum(quarentae5a49$DIF)/nrow(quarentae5a49)*5

cinquentaa54 <- call(um,sexo1pe,c("50","51","52","53","54"))
nkx50a54 <- sum(cinquentaa54$DIF)/nrow(cinquentaa54)*5

cinquentae5a59 <- call(um,sexo1pe,c("55","56","57","58","59"))
nkx55a59 <- sum(cinquentae5a59$DIF)/nrow(cinquentae5a59)*5

sessentaa64 <- call(um,sexo1pe,c("60","61","62","63","64"))
nkx60a64 <- sum(sessentaa64$DIF)/nrow(sessentaa64)*5

sessentae5a69 <- call(um,sexo1pe,c("65","66","67","68","69"))
nkx65a69 <- sum(sessentae5a69$DIF)/nrow(sessentae5a69)*5

setentaa74 <- call(um,sexo1pe,c("70","71","72","73","74"))
nkx70a74 <- sum(setentaa74$DIF)/nrow(setentaa74)*5

setentae5a79 <- call(um,sexo1pe,c("75","76","77","78","79"))
nkx75a79 <- sum(setentae5a79$DIF)/nrow(setentae5a79)*5

oitentaa84 <- call(um,sexo1pe,c("80","81","82","83","84"))
nkx80a84 <- sum(oitentaa84$DIF)/nrow(oitentaa84)*5

oitentae5a89 <- call(um,sexo1pe,c("85","86","87","88","89"))
nkx85a89 <- sum(oitentae5a89$DIF)/nrow(oitentae5a89)*5


noventamais <- call(um,sexo1pe,c("90","91","92","93","94",
                                 "95","96","97","98","99"))
nkx90mais <- sum(noventamais$DIF)/nrow(noventamais)*10


nkxpesexo1 <- c(nkx0,nkx1a4,nkx5a9,nkx10a14,nkx15a19,nkx20a24,nkx25a29,nkx30a34,
                nkx35a39,nkx40a44,nkx45a49,nkx50a54,nkx55a59,nkx60a64,nkx65a69,
                nkx70a74,nkx75a79,nkx80a84,nkx85a89,nkx90mais)
nkxpesexo1 <- as.data.frame(nkxpesexo1)

# write.csv(nkxpesexo1,"nkxpesexo1.csv")

#------------------------------------------------------------------------------

sexo2pe <- DOPE2 %>%
  select(IDADE, SEXO,DIF) %>%
  filter(SEXO == "2") %>%
  select(IDADE,DIF)

zero <- call(um,sexo2pe,"0")
nkx0 <- sum(zero$DIF)/nrow(zero)

uma4 <- call(um,sexo2pe,c("1","2","3","4"))
nkx1a4 <- sum(uma4$DIF)/nrow(uma4)*4

cincoa9 <- call(um,sexo2pe,c("5","6","7","8","9"))
nkx5a9 <- sum(cincoa9$DIF)/nrow(cincoa9)*5

deza14 <- call(um,sexo2pe,c("10","11","12","13","14"))
nkx10a14 <- sum(deza14$DIF)/nrow(deza14)*5

quinzea19 <- call(um,sexo2pe,c("15","16","17","18","19"))
nkx15a19 <- sum(quinzea19$DIF)/nrow(quinzea19)*5

vintea24 <- call(um,sexo2pe,c("20","21","22","23","24"))
nkx20a24 <- sum(vintea24$DIF)/nrow(vintea24)*5

vinte5a29 <- call(um,sexo2pe,c("25","26","27","28","29"))
nkx25a29 <- sum(vinte5a29$DIF)/nrow(vinte5a29)*5

trintaa34 <- call(um,sexo2pe,c("30","31","32","33","34"))
nkx30a34 <- sum(trintaa34$DIF)/nrow(trintaa34)*5

trintae5a39 <- call(um,sexo2pe,c("35","36","37","38","39"))
nkx35a39 <- sum(trintae5a39$DIF)/nrow(trintae5a39)*5

quarentaa44 <- call(um,sexo2pe,c("40","41","42","43","44"))
nkx40a44 <- sum(quarentaa44$DIF)/nrow(quarentaa44)*5

quarentae5a49 <- call(um,sexo2pe,c("45","46","47","48","49"))
nkx45a49 <- sum(quarentae5a49$DIF)/nrow(quarentae5a49)*5

cinquentaa54 <- call(um,sexo2pe,c("50","51","52","53","54"))
nkx50a54 <- sum(cinquentaa54$DIF)/nrow(cinquentaa54)*5

cinquentae5a59 <- call(um,sexo2pe,c("55","56","57","58","59"))
nkx55a59 <- sum(cinquentae5a59$DIF)/nrow(cinquentae5a59)*5

sessentaa64 <- call(um,sexo2pe,c("60","61","62","63","64"))
nkx60a64 <- sum(sessentaa64$DIF)/nrow(sessentaa64)*5

sessentae5a69 <- call(um,sexo2pe,c("65","66","67","68","69"))
nkx65a69 <- sum(sessentae5a69$DIF)/nrow(sessentae5a69)*5

setentaa74 <- call(um,sexo2pe,c("70","71","72","73","74"))
nkx70a74 <- sum(setentaa74$DIF)/nrow(setentaa74)*5

setentae5a79 <- call(um,sexo2pe,c("75","76","77","78","79"))
nkx75a79 <- sum(setentae5a79$DIF)/nrow(setentae5a79)*5

oitentaa84 <- call(um,sexo2pe,c("80","81","82","83","84"))
nkx80a84 <- sum(oitentaa84$DIF)/nrow(oitentaa84)*5

oitentae5a89 <- call(um,sexo2pe,c("85","86","87","88","89"))
nkx85a89 <- sum(oitentae5a89$DIF)/nrow(oitentae5a89)*5


noventamais <- call(um,sexo2pe,c("90","91","92","93","94",
                                 "95","96","97","98","99"))
nkx90mais <- sum(noventamais$DIF)/nrow(noventamais)*10


nkxpesexo2 <- c(nkx0,nkx1a4,nkx5a9,nkx10a14,nkx15a19,nkx20a24,nkx25a29,nkx30a34,
                nkx35a39,nkx40a44,nkx45a49,nkx50a54,nkx55a59,nkx60a64,nkx65a69,
                nkx70a74,nkx75a79,nkx80a84,nkx85a89,nkx90mais)
nkxpesexo2 <- as.data.frame(nkxpesexo2)

# write.csv(nkxpesexo2,"nkxpesexo2.csv")

#------------------------------------------------------------------------------

