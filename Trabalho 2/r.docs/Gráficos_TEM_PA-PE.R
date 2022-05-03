library(ggplot2)

i <- c("0","01-04","05-09","10-14","15-19","20-24","25-29","30-34","35-39"
       ,"40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79"
       ,"80-84","85-89","90+")

N <- 100000
  
#--------------------TEM 2020+2021 PA+PE----------------------------------------

# tempefem <- c(397.6918863,563.0167222,159.4805987,108.8949136,119.4686415,
#              126.8991755,163.2168182,222.118031,315.89149,502.1276481,
#              724.1467564,1117.127349,1688.934186,2501.720458,3718.854163,
#              5563.789261,8789.775011,14006.09756,22085.01755,40938.16631)
#
# tempemasc <- c(199.0363205,519.0502879,147.3432432,92.56885314,417.529591,
#               613.1219815,622.7153328,679.7918371,811.4674845,1068.954299,
#               1374.062846,2075.40307,2912.04975,4248.404959,6252.701785,
#               8979.18361,13287.20909,19568.38143,29018.1228,56769.7461)
#
# tempafem <- c(98.92182329,278.6746994,96.6163193,51.36941423,64.88766323,
#  70.80399671,87.3222855,105.6684341,160.9797089,225.3764812,305.6255182,
#  491.0671862,719.3285477,1167.706497,1871.446018,2807.311117,4289.069381,
#  7157.678017,11045.25039,17067.22396)
#
# tempamasc <- c(102.0304051,931.327781,289.8765067,191.3302089,508.7684739,
#               841.8486043,834.346394,876.0281221,1028.241692,1286.617303,
#               1680.489474,2543.569526,3696.523046,5720.102622,9138.638558,
#               13804.82375,20853.55917,31741.67094,46356.29289,70114.00651)

#------------------------------TEM 2019,2020,21 PA FEM--------------------------

tempafem2019 <- c(
  0.000933298,
  0.0028889,
  0.0009321,
  0.0005780,
  0.0005914,
  0.0007020,
  0.0008230,
  0.0008681,
  0.0013489,
  0.0017126,
  0.0023014,
  0.0035429,
  0.0057242,
  0.0088312,
  0.0135276,
  0.0217085,
  0.0347819,
  0.0582387,
  0.0958045,
  0.1488485
  
)
tempafem2019 <- tempafem2019*100000

tempafem2020 <- c(
  0.001036829,
  0.0029239,
  0.0009657,
  0.0005426,
  0.0006600,
  0.0006924,
  0.0008797,
  0.0010295,
  0.0015354,
  0.0020956,
  0.0027700,
  0.0047858,
  0.0066683,
  0.0114440,
  0.0176322,
  0.0279817,
  0.0440531,
  0.0761170,
  0.1147736,
  0.1771818
  
)
tempafem2020 <- tempafem2020*100000

tempafem2021 <- c(
  0.000941405,
  0.0026499,
  0.0009666,
  0.0004843,
  0.0006377,
  0.0007237,
  0.0008668,
  0.0010838,
  0.0016832,
  0.0024069,
  0.0033326,
  0.0050314,
  0.0076996,
  0.0119026,
  0.0197497,
  0.0281604,
  0.0417707,
  0.0672470,
  0.1063258,
  0.1644236
  
)
tempafem2021 <- tempafem2021*100000

#------------------------------------------------------------------------------

#------------------------------TEM 2019,2020,21 PA MASC------------------------

tempamasc2019 <- c(
  0.000594763,
  0.0033012,
  0.0010483,
  0.0006393,
  0.0020904,
  0.0030813,
  0.0029515,
  0.0029185,
  0.0031046,
  0.0034920,
  0.0041620,
  0.0062257,
  0.0096299,
  0.0143687,
  0.0224554,
  0.0335346,
  0.0546754,
  0.0801213,
  0.1302471,
  0.2020548
  
)
tempamasc2019 <- tempamasc2019*N

tempamasc2020 <- c(
  0.000638058,
  0.0031355,
  0.0010009,
  0.0006534,
  0.0018636,
  0.0028356,
  0.0027383,
  0.0029940,
  0.0032826,
  0.0040412,
  0.0054439,
  0.0079748,
  0.0126423,
  0.0193450,
  0.0315104,
  0.0487942,
  0.0756452,
  0.1178806,
  0.1707080,
  0.2580378
  
)
tempamasc2020 <- tempamasc2020*N

tempamasc2021 <- c(
  0.000381711,
  0.0030870,
  0.0009511,
  0.0006328,
  0.0016136,
  0.0027912,
  0.0027942,
  0.0028795,
  0.0034886,
  0.0043814,
  0.0056351,
  0.0086610,
  0.0120611,
  0.0187899,
  0.0296588,
  0.0442259,
  0.0659847,
  0.0990179,
  0.1453181,
  0.2199808
  
)
tempamasc2021 <- tempamasc2021*N

#------------------------------------------------------------------------------

#------------------------------TEM 2019,2020,21 PE FEM--------------------------

tempefem2019 <- c(
  0.001984127,
  0.002695368,
  0.000851675,
  0.000544275,
  0.000505862,
  0.000643945,
  0.000680104,
  0.001225787,
  0.001244325,
  0.002031088,
  0.00268109,
  0.004306768,
  0.006603921,
  0.009528699,
  0.015425295,
  0.023245793,
  0.038440453,
  0.062412534,
  0.106465338,
  0.17630662
  
)
tempefem2019 <- tempefem2019*N

tempefem2020 <- c(
  0.002195964,
  0.002692122,
  0.000758444,
  0.000608774,
  0.000616995,
  0.000638545,
  0.000784383,
  0.003104266,
  0.001422266,
  0.002415393,
  0.003337934,
  0.005309189,
  0.007871415,
  0.011970455,
  0.018156015,
  0.027674536,
  0.044438772,
  0.071513688,
  0.113263376,
  0.188701055
  
)
tempefem2020 <- tempefem2020*N

tempefem2021 <- c(
  0.001779038,
  0.002938171,
  0.000836635,
  0.000479188,
  0.000577344,
  0.00063042,
  0.000847782,
  0.00039924,
  0.001735648,
  0.002604382,
  0.003898188,
  0.005857369,
  0.009002302,
  0.013029959,
  0.01902036,
  0.027958973,
  0.043476924,
  0.068591082,
  0.10774808,
  0.220170653
  
)
tempefem2021 <- tempefem2021*N
#------------------------------------------------------------------------------

#------------------------------TEM 2019,2020,21 PE MASC------------------------

tempemasc2019 <- c(
  0.000833722,
  0.002622353,
  0.000772556,
  0.000599619,
  0.002124865,
  0.002975908,
  0.00291589,
  0.003024961,
  0.003326471,
  0.004289304,
  0.005525703,
  0.008371403,
  0.011792702,
  0.016938327,
  0.025344379,
  0.0359732,
  0.058186894,
  0.083570525,
  0.134563275,
  0.235154159
  
)
tempemasc2019 <- tempemasc2019*N

tempemasc2020 <- c(
  0.001083331,
  0.002576345,
  0.000788227,
  0.000452902,
  0.002117477,
  0.003266828,
  0.003216492,
  0.003380839,
  0.003934698,
  0.004989975,
  0.006621965,
  0.009800649,
  0.013884936,
  0.020846531,
  0.031440178,
  0.045203292,
  0.068169892,
  0.101669041,
  0.152462384,
  0.265030795
  
)
tempemasc2020 <- tempemasc2020*N

tempemasc2021 <- c(
  0.000906227,
  0.002614174,
  0.000684865,
  0.000472928,
  0.002057359,
  0.00286348,
  0.003011284,
  0.003417159,
  0.004179076,
  0.005693576,
  0.007113386,
  0.0109428,
  0.015216731,
  0.021623443,
  0.031092296,
  0.044596468,
  0.064770401,
  0.09411823,
  0.138087452,
  0.233944069
  
)
tempemasc2021 <- tempemasc2021*N
#------------------------------------------------------------------------------

# tempefemdf <- as.data.frame(tempefem,i)
# tempemascdf <- as.data.frame(tempemasc,i)
# tempafemdf <- as.data.frame(tempafem,i)
# tempamascdf <- as.data.frame(tempamasc,i)



tempafem2019df <- as.data.frame(tempafem2019,i)
tempafem2020df <- as.data.frame(tempafem2020,i)
tempafem2021df <- as.data.frame(tempafem2021,i)

tempamasc2019df <- as.data.frame(tempamasc2019,i)
tempamasc2020df <- as.data.frame(tempamasc2020,i)
tempamasc2021df <- as.data.frame(tempamasc2021,i)

tempefem2019df <- as.data.frame(tempefem2019,i)
tempefem2020df <- as.data.frame(tempefem2020,i)
tempefem2021df <- as.data.frame(tempefem2021,i)

tempemasc2019df <- as.data.frame(tempemasc2019,i)
tempemasc2020df <- as.data.frame(tempemasc2020,i)
tempemasc2021df <- as.data.frame(tempemasc2021,i)



#------------------- Pernambuco -----------------------------------------------

# 2019

ggplot(tempefem2019df, aes(y=tempefem2019,x=i,log="y", group=1))+
  geom_line(aes(col="Feminino"))+
  geom_point(data=tempefem2019df,aes(y=tempefem2019,col="Feminino"))+ 
  scale_y_continuous(trans='log2')+
  labs(title="Taxas Específicas de Mortalidade - Pernambuco 2019"
       ,x="Faixa de idade", y="Taxa específica de Mortalidade")+
  geom_line(data=tempemasc2019df,aes(y=tempemasc2019,col="Masculino"))+
  geom_point(data=tempemasc2019df,aes(y=tempemasc2019,col="Masculino"))

# 2020

ggplot(tempefem2020df, aes(y=tempefem2020,x=i,log="y", group=1))+
  geom_line(aes(col="Feminino"))+
  geom_point(data=tempefem2020df,aes(y=tempefem2020,col="Feminino"))+ 
  scale_y_continuous(trans='log2')+
  labs(title="Taxas Específicas de Mortalidade - Pernambuco 2020"
       ,x="Faixa de idade", y="Taxa específica de Mortalidade")+
  geom_line(data=tempemasc2020df,aes(y=tempemasc2020,col="Masculino"))+
  geom_point(data=tempemasc2020df,aes(y=tempemasc2020,col="Masculino"))

# 2021

ggplot(tempefem2021df, aes(y=tempefem2021,x=i,log="y", group=1))+
  geom_line(aes(col="Feminino"))+
  geom_point(data=tempefem2021df,aes(y=tempefem2021,col="Feminino"))+ 
  scale_y_continuous(trans='log2')+
  labs(title="Taxas Específicas de Mortalidade - Pernambuco 2021"
       ,x="Faixa de idade", y="Taxa específica de Mortalidade")+
  geom_line(data=tempemasc2021df,aes(y=tempemasc2021,col="Masculino"))+
  geom_point(data=tempemasc2021df,aes(y=tempemasc2021,col="Masculino"))

#------------------- Pará -----------------------------------------------------

# 2019

ggplot(tempafem2019df, aes(y=tempafem2019,x=i,log="y", group=1))+
  geom_line(aes(col="Feminino"))+
  geom_point(data=tempafem2019df,aes(y=tempafem2019,col="Feminino"))+ 
  scale_y_continuous(trans='log2')+
  labs(title="Taxas Específicas de Mortalidade - Pará 2019"
       ,x="Faixa de idade", y="Taxa específica de Mortalidade")+
  geom_line(data=tempamasc2019df,aes(y=tempamasc2019,col="Masculino"))+
  geom_point(data=tempamasc2019df,aes(y=tempamasc2019,col="Masculino"))

# 2020

ggplot(tempafem2020df, aes(y=tempafem2020,x=i,log="y", group=1))+
  geom_line(aes(col="Feminino"))+
  geom_point(data=tempafem2020df,aes(y=tempafem2020,col="Feminino"))+ 
  scale_y_continuous(trans='log2')+
  labs(title="Taxas Específicas de Mortalidade - Pará 2020"
       ,x="Faixa de idade", y="Taxa específica de Mortalidade")+
  geom_line(data=tempamasc2020df,aes(y=tempamasc2020,col="Masculino"))+
  geom_point(data=tempamasc2020df,aes(y=tempamasc2020,col="Masculino"))

# 2021

ggplot(tempafem2021df, aes(y=tempafem2021,x=i,log="y", group=1))+
  geom_line(aes(col="Feminino"))+
  geom_point(data=tempafem2021df,aes(y=tempafem2021,col="Feminino"))+ 
  scale_y_continuous(trans='log2')+
  labs(title="Taxas Específicas de Mortalidade - Pará 2021"
       ,x="Faixa de idade", y="Taxa específica de Mortalidade")+
  geom_line(data=tempamasc2021df,aes(y=tempamasc2021,col="Masculino"))+
  geom_point(data=tempamasc2021df,aes(y=tempamasc2021,col="Masculino"))

#------------------------------------------------------------------------------
