library(XML)
library(reshape2)
library(plyr)
library(ggplot2)

projPE100ANOS

ggplot(projPE100ANOS, aes(x = idade, fill = sexo,
                          y = ifelse(test = sexo == "masc",
                                     yes = -a2010, no = a2010))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(projPE100ANOS$a2010) * c(-1,1)) +
  labs(title = "Pirâmide populacional de Pernambuco - 2010", x = "Idade", y = "Porcentagem da população") +
  coord_flip()

ggplot(projPE100ANOS, aes(x = idade, fill = sexo,
                          y = ifelse(test = sexo == "masc",
                                     yes = -a2015, no = a2015))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(projPE100ANOS$a2015) * c(-1,1)) +
  labs(title = "Pirâmide populacional de Pernambuco - 2015", x = "Idade", y = "Porcentagem da população") +
  coord_flip()

ggplot(projPE100ANOS, aes(x = idade, fill = sexo,
                          y = ifelse(test = sexo == "masc",
                                     yes = -a2020, no = a2020))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(projPE100ANOS$a2020) * c(-1,1)) +
  labs(title = "Pirâmide populacional de Pernambuco - 2020", x = "Idade", y = "Porcentagem da população") +
  coord_flip()

ggplot(projPE100ANOS, aes(x = idade, fill = sexo,
                          y = ifelse(test = sexo == "masc",
                                     yes = -a2025, no = a2025))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(projPE100ANOS$a2025) * c(-1,1)) +
  labs(title = "Pirâmide populacional de Pernambuco - 2025", x = "Idade", y = "Porcentagem da população") +
  coord_flip()

ggplot(projPE100ANOS, aes(x = idade, fill = sexo,
                          y = ifelse(test = sexo == "masc",
                                     yes = -a2030, no = a2030))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(projPE100ANOS$a2030) * c(-1,1)) +
  labs(title = "Pirâmide populacional de Pernambuco - 2030", x = "Idade", y = "Porcentagem da população") +
  coord_flip()

ggplot(projPE100ANOS, aes(x = idade, fill = sexo,
                          y = ifelse(test = sexo == "masc",
                                     yes = -a2035, no = a2035))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(projPE100ANOS$a2035) * c(-1,1)) +
  labs(title = "Pirâmide populacional de Pernambuco - 2035", x = "Idade", y = "Porcentagem da população") +
  coord_flip()

ggplot(projPE100ANOS, aes(x = idade, fill = sexo,
                          y = ifelse(test = sexo == "masc",
                                     yes = -a2040, no = a2040))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(projPE100ANOS$a2040) * c(-1,1)) +
  labs(title = "Pirâmide populacional de Pernambuco - 2040", x = "Idade", y = "Porcentagem da população") +
  coord_flip()

ggplot(projPE100ANOS, aes(x = idade, fill = sexo,
                          y = ifelse(test = sexo == "masc",
                                     yes = -a2045, no = a2045))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(projPE100ANOS$a2045) * c(-1,1)) +
  labs(title = "Pirâmide populacional de Pernambuco - 2045", x = "Idade", y = "Porcentagem da população") +
  coord_flip()

ggplot(projPE100ANOS, aes(x = idade, fill = sexo,
                          y = ifelse(test = sexo == "masc",
                                     yes = -a2050, no = a2050))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(projPE100ANOS$a2050) * c(-1,1)) +
  labs(title = "Pirâmide populacional de Pernambuco - 2050", x = "Idade", y = "Porcentagem da população") +
  coord_flip()

ggplot(projPE100ANOS, aes(x = idade, fill = sexo,
                          y = ifelse(test = sexo == "masc",
                                     yes = -a2055, no = a2055))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(projPE100ANOS$a2055) * c(-1,1)) +
  labs(title = "Pirâmide populacional de Pernambuco - 2055", x = "Idade", y = "Porcentagem da população") +
  coord_flip()

ggplot(projPE100ANOS, aes(x = idade, fill = sexo,
                          y = ifelse(test = sexo == "masc",
                                     yes = -a2060, no = a2060))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(projPE100ANOS$a2060) * c(-1,1)) +
  labs(title = "Pirâmide populacional de Pernambuco - 2060", x = "Idade", y = "Porcentagem da população") +
  coord_flip()

ggplot(projPE100ANOS, aes(x = idade, fill = sexo,
                          y = ifelse(test = sexo == "masc",
                                     yes = -a2065, no = a2065))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(projPE100ANOS$a2065) * c(-1,1)) +
  labs(title = "Pirâmide populacional de Pernambuco - 2065", x = "Idade", y = "Porcentagem da população") +
  coord_flip()

ggplot(projPE100ANOS, aes(x = idade, fill = sexo,
                          y = ifelse(test = sexo == "masc",
                                     yes = -a2070, no = a2070))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(projPE100ANOS$a2070) * c(-1,1)) +
  labs(title = "Pirâmide populacional de Pernambuco - 2070", x = "Idade", y = "Porcentagem da população") +
  coord_flip()

ggplot(projPE100ANOS, aes(x = idade, fill = sexo,
                          y = ifelse(test = sexo == "masc",
                                     yes = -a2075, no = a2075))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(projPE100ANOS$a2075) * c(-1,1)) +
  labs(title = "Pirâmide populacional de Pernambuco - 2075", x = "Idade", y = "Porcentagem da população") +
  coord_flip()

ggplot(projPE100ANOS, aes(x = idade, fill = sexo,
                          y = ifelse(test = sexo == "masc",
                                     yes = -a2080, no = a2080))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(projPE100ANOS$a2080) * c(-1,1)) +
  labs(title = "Pirâmide populacional de Pernambuco - 2080", x = "Idade", y = "Porcentagem da população") +
  coord_flip()

ggplot(projPE100ANOS, aes(x = idade, fill = sexo,
                          y = ifelse(test = sexo == "masc",
                                     yes = -a2085, no = a2085))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(projPE100ANOS$a2085) * c(-1,1)) +
  labs(title = "Pirâmide populacional de Pernambuco - 2085", x = "Idade", y = "Porcentagem da população") +
  coord_flip()

ggplot(projPE100ANOS, aes(x = idade, fill = sexo,
                          y = ifelse(test = sexo == "masc",
                                     yes = -a2090, no = a2090))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(projPE100ANOS$a2090) * c(-1,1)) +
  labs(title = "Pirâmide populacional de Pernambuco - 2090", x = "Idade", y = "Porcentagem da população") +
  coord_flip()

ggplot(projPE100ANOS, aes(x = idade, fill = sexo,
                          y = ifelse(test = sexo == "masc",
                                     yes = -a2095, no = a2095))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(projPE100ANOS$a2095) * c(-1,1)) +
  labs(title = "Pirâmide populacional de Pernambuco - 2095", x = "Idade", y = "Porcentagem da população") +
  coord_flip()

ggplot(projPE100ANOS, aes(x = idade, fill = sexo,
                          y = ifelse(test = sexo == "masc",
                                     yes = -a2100, no = a2100))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(projPE100ANOS$a2100) * c(-1,1)) +
  labs(title = "Pirâmide populacional de Pernambuco - 2100", x = "Idade", y = "Porcentagem da população") +
  coord_flip()

ggplot(projPE100ANOS, aes(x = idade, fill = sexo,
                          y = ifelse(test = sexo == "masc",
                                     yes = -a2105, no = a2105))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(projPE100ANOS$a2105) * c(-1,1)) +
  labs(title = "Pirâmide populacional de Pernambuco - 2105", x = "Idade", y = "Porcentagem da população") +
  coord_flip()

ggplot(projPE100ANOS, aes(x = idade, fill = sexo,
                          y = ifelse(test = sexo == "masc",
                                     yes = -a2110, no = a2110))) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = max(projPE100ANOS$a2110) * c(-1,1)) +
  labs(title = "Pirâmide populacional de Pernambuco - 2110", x = "Idade", y = "Porcentagem da população") +
  coord_flip()