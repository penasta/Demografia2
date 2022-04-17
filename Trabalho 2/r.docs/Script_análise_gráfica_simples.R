library(readr)
library(ggplot2)
library(dplyr)

DOPA <- read_csv("Trabalho 2/Bancos/SIM/DOPA.csv")

summary(DOPA)

# Analisando a contagem de mortes pelo capítulo CID

df <- DOPA %>%
  group_by(capcid10) %>%
  summarise(counts = n())
df

ggplot(df, aes(x = capcid10, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()

# Analisando a contagem de mortes pelo sexo

df2 <- DOPA %>%
  group_by(Sexo) %>%
  summarise(counts = n())
df2

ggplot(df2, aes(x = Sexo, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()

# Analisando a contagem de mortes pela idade

df3 <- DOPA %>%
  group_by(IDADE) %>%
  summarise(counts = n())
df3

ggplot(df3, aes(x = IDADE, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()

# Analisando a contagem de mortes pela raça/cor

df4 <- DOPA %>%
  group_by(RACACOR) %>%
  summarise(counts = n())
df4

ggplot(df4, aes(x = RACACOR, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()

# Contagem simples de mortes pelo ano de ocorrência

df5 <- DOPA %>%
  group_by(ANO) %>%
  summarise(counts = n())
df5

ggplot(df5, aes(x = ANO, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()