library(readxl)
library(tidyverse)

Banco_Fertilidade <- read_excel("Bancos/Banco_Fertilidade.xlsx", sheet='FERTILITY INDICATORS')
fert <- Banco_Fertilidade
rm(Banco_Fertilidade)

# 4x
names(fert) <- fert[1,]
fert <- fert[-1,]

fert$`Country or Area Code` <- NULL
fert$Indicator <- NULL
fert$Date <- NULL
fert$Series <- NULL
fert$DataType <- NULL
fert$`Data Source Type` <- NULL
fert$`Survey Programme` <- NULL
fert$`Data Source Inventory ID` <- NULL
fert$`Data Source Name` <- NULL
fert$`Data Source Name (short)` <- NULL
fert$`Data Source Start Year` <- NULL
fert$`Data Source End Year` <- NULL
fert$Reference <- NULL

# ecuador 21836-22344
# greece 29504-30036
# togo 70518-70827

fert <- fert[c(21836:22344, 29504:30036, 70518:70827), ]

# ecuador 1-509
# greece 510-1042
# togo 1043-1352

summary(fert)

fert$Value <- as.numeric(fert$Value)
fert$`Reference Year` <- as.integer(fert$`Reference Year`)

summary(fert)

ferteq <- fert[c(1:509),]
fertgr <- fert[c(510:1042),]
fertto <- fert[c(1043:1352),]

# ferteq <- fert[order(ferteq$`Reference Year`),]
# fertgr <- fert[order(fertgr$`Reference Year`),]
# fertto <- fert[order(fertto$`Reference Year`),]
