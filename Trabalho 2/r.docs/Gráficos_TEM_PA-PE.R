library(ggplot2)
# library(reshape2)


i <- c("0","01-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39"
       ,"40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79"
       ,"80-84","85-89","90+")
  

tempefem <- c(397.6918863,563.0167222,159.4805987,108.8949136,119.4686415,
              126.8991755,163.2168182,222.118031,315.89149,502.1276481,
              724.1467564,1117.127349,1688.934186,2501.720458,3718.854163,
              5563.789261,8789.775011,14006.09756,22085.01755,40938.16631)

tempemasc <- c(199.0363205,519.0502879,147.3432432,92.56885314,417.529591,
               613.1219815,622.7153328,679.7918371,811.4674845,1068.954299,
               1374.062846,2075.40307,2912.04975,4248.404959,6252.701785,
               8979.18361,13287.20909,19568.38143,29018.1228,56769.7461)

tempafem <- c(98.92182329,278.6746994,96.6163193,51.36941423,64.88766323,
  70.80399671,87.3222855,105.6684341,160.9797089,225.3764812,305.6255182,
  491.0671862,719.3285477,1167.706497,1871.446018,2807.311117,4289.069381,
  7157.678017,11045.25039,17067.22396)

tempamasc <- c(102.0304051,931.327781,289.8765067,191.3302089,508.7684739,
               841.8486043,834.346394,876.0281221,1028.241692,1286.617303,
               1680.489474,2543.569526,3696.523046,5720.102622,9138.638558,
               13804.82375,20853.55917,31741.67094,46356.29289,70114.00651)


tempefemdf <- as.data.frame(tempefem,i)
tempemascdf <- as.data.frame(tempemasc,i)
tempafemdf <- as.data.frame(tempafem,i)
tempamascdf <- as.data.frame(tempamasc,i)

#------------------- Pernambuco -----------------------------------------------

ggplot(tempefemdf, aes(y=tempefem,x=i,log="y", group=1))+
  geom_line(aes(col="Feminino"))+
  geom_point(data=tempefemdf,aes(y=tempefem,col="Feminino"))+ 
  scale_y_continuous(trans='log2')+
  labs(title="Taxas Específicas de Mortalidade - Pernambuco"
       ,x="Faixa de idade", y="Taxa específica de Mortalidade")+
  geom_line(data=tempemascdf,aes(y=tempemasc,col="Masculino"))+
  geom_point(data=tempemascdf,aes(y=tempemasc,col="Masculino"))

#------------------- Pará -----------------------------------------------------

ggplot(tempafemdf, aes(y=tempafem,x=i,log="y", group=1))+
  geom_line(aes(col="Feminino"))+
  geom_point(data=tempafemdf,aes(y=tempafem,col="Feminino"))+ 
  scale_y_continuous(trans='log2')+
  labs(title="Taxas Específicas de Mortalidade - Pará"
       ,x="Faixa de idade", y="Taxa específica de Mortalidade")+
  geom_line(data=tempamascdf,aes(y=tempamasc,col="Masculino"))+
  geom_point(data=tempamascdf,aes(y=tempamasc,col="Masculino"))

#------------------------------------------------------------------------------
