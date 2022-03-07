library(readr)
leq <- read_csv("Bancos/data_ecuador.csv")
View(leq)

summary(leq)

leq2 <- leq[, c(2,15:17)]

names(leq2) <- leq2[1,]
leq2 <- leq2[-1,]

summary(leq2)

leq2$`Both sexes`<- as.numeric(leq2$`Both sexes`)
leq2$Male<- as.numeric(leq2$Male)
leq2$Female<- as.numeric(leq2$Female)

banco <- leq2
leq2 <- banco

leq2 <- leq2[, c(2:4)]

rownames(leq2) <- banco$`Age Group`

library(data.table)

leq2 <- as.data.frame(leq2)

leq3 <- transpose(leq2)

colnames(leq3) <- rownames(leq2)
rownames(leq3) <- colnames(leq2)

summary(leq3)

leq3$`0-4 years`<-leq3$`1-4 years`+leq3$`<1 year`
leq3$`80+ years`<-leq3$`80-84 years`+leq3$`85+ years`

leq3$`1-4 years`<-NULL
leq3$`<1 year`<-NULL
leq3$`80-84 years`<-NULL
leq3$`85+ years`<-NULL

library(tibble)
data <- as_data_frame(leq3)

colnames(data)

data2 <- data[, c( "0-4 years"  ,"5-9 years"  ,  "10-14 years"  ,"15-19  years", "20-24 years"  ,"25-29 years",
                   "30-34 years"  ,"35-39 years", "40-44 years" , "45-49 years" , "50-54 years" , "55-59 years",
                   "60-64 years",  "65-69 years",  "70-74 years", "75-79 years","80+ years"  )]

nLxequador<-data2

saveRDS(nLxequador, file = "Bancos/nLxequador.rds")

