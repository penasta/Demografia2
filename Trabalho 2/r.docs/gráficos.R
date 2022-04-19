####################### Instalando os pacotes #######################

if (!require(pacman)) {
  install.packages('pacman')
  library(pacman)
}

pacman::p_load(read.dbc,dplyr, stringr, lubridate, LexisPlotR, ggplot2, readxl, foreign,maptools,plotly)

library(readr)

####################### Lendo os dados #######################
# DOPA <- read.csv("C:/Users/emill/OneDrive/Documentos/DOPA.csv", sep = ',')
# DOPE <- read.csv("C:/Users/emill/OneDrive/Documentos/DOPE.csv", sep = ',')

proj_PA_H <- readxl::read_xls("Trabalho 2/Bancos/PROJECOES_2013_POPULACAO.xls",
                              range = c("U5:W25"),sheet = "PA")

proj_PA_M <- readxl::read_xls("Trabalho 2/Bancos/PROJECOES_2013_POPULACAO.xls",
                              range = c("U28:W48"),sheet = "PA")

proj_PE_H <- readxl::read_xls("Trabalho 2/Bancos/PROJECOES_2013_POPULACAO.xls",
                              range = c("U5:W25"),sheet = "PE")

proj_PE_M <- readxl::read_xls("Trabalho 2/Bancos/PROJECOES_2013_POPULACAO.xls",
                              range = c("U28:W48"),sheet = "PE")

DOPA <- read_csv("Trabalho 2/Bancos/SIM/DOPA.csv")
DOPE <- read_csv("Trabalho 2/Bancos/SIM/DOPE.csv")


# Categorizacao da idade

DOPA$idade_cat <- NA
DOPA$idade_cat <- cut(DOPA$IDADE,
                      breaks=c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,
                               65,70,75,80,85,200),
                      labels=c("<1","1-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                               "40-44","45-49","50-54","55-59","60-64","65-69",
                               "70-74","75-79","80-84","85+"),
                      include.lowest = TRUE, right = FALSE, ordered_result = TRUE)


DOPE$idade_cat <- NA
DOPE$idade_cat <- cut(DOPE$IDADE,
                      breaks=c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,
                               65,70,75,80,85,200),
                      labels=c("<1","1-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39",
                               "40-44","45-49","50-54","55-59","60-64","65-69",
                               "70-74","75-79","80-84","85+"),
                      include.lowest = TRUE, right = FALSE, ordered_result = TRUE)


################### Para ###################

# Sexo 

DOPA$ANO <- factor(as.character(DOPA$ANO), 
                                 levels = c('2019','2020', '2021'))


ggplot(data = DOPA %>% filter(Sexo != "Ignorado"), 
       aes(x = reorder(ANO, ANO, function(x) -length(x)), 
           fill = Sexo, text = paste("Sexo: ",Sexo, "<br>",
                                     " Ano:", as.factor(ANO)))) +
  geom_bar(position = "dodge", show.legend = TRUE, color = 'black', 
           na.rm = TRUE, alpha = 0.5, width = 0.8) +
  theme_gray() + 
  ggtitle("")+
  theme(legend.position='none') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())+
  scale_x_discrete(na.translate = FALSE)+
  scale_fill_brewer(palette = "Set1") -> sexo_PA

ggplotly(sexo_PA, tooltip = c("count","text"))


# Idade conjunto

ggplot(data = DOPA %>% filter(is.na(idade_cat) == FALSE)) +
  geom_bar(aes(y = idade_cat, fill = ANO,
               text = paste("Ano: ",ANO, 
                            "<br>",
                            "Faixa Etaria :", as.factor(idade_cat))), 
           position = "stack", show.legend = TRUE, color = 'black', 
           na.rm = TRUE, alpha = 0.5) +
  theme_gray() + 
  theme(legend.position='none') +
  ggtitle("") +
  ylab("") +
  xlab("") +
  scale_fill_brewer(direction=-1,palette = "Set2") -> idade_PA

ggplotly(idade_PA,
         height = 860, tooltip = c("count","text"))


# Idade 2019

ggplot(data = DOPA %>% filter(is.na(idade_cat) == FALSE,
                              ANO == '2019')) +
  geom_bar(aes(y = idade_cat, fill = ANO,
               text = paste("Ano: ",ANO, 
                            "<br>",
                            "Faixa Etaria :", as.factor(idade_cat))), 
           position = "stack", show.legend = TRUE, color = 'black', 
           na.rm = TRUE, alpha = 0.5) +
  theme_gray() + 
  theme(legend.position='none') +
  ggtitle("") +
  ylab("") +
  xlab("") +
  scale_fill_brewer(direction=-1,palette = "Set2") -> idade_PA_19

ggplotly(idade_PA_19,
         height = 860, tooltip = c("count","text"))


# Idade 2020

ggplot(data = DOPA %>% filter(is.na(idade_cat) == FALSE,
                              ANO == '2020')) +
  geom_bar(aes(y = idade_cat, fill = ANO,
               text = paste("Ano: ",ANO, 
                            "<br>",
                            "Faixa Etaria :", as.factor(idade_cat))), 
           position = "stack", show.legend = TRUE, color = 'black', 
           na.rm = TRUE, alpha = 0.5) +
  theme_gray() + 
  theme(legend.position='none') +
  ggtitle("") +
  ylab("") +
  xlab("") +
  scale_fill_brewer(direction=-1,palette = "Set2") -> idade_PA_20

ggplotly(idade_PA_20,
         height = 860, tooltip = c("count","text"))


# Idade 2021

ggplot(data = DOPA %>% filter(is.na(idade_cat) == FALSE,
                              ANO == '2021')) +
  geom_bar(aes(y = idade_cat, fill = ANO,
               text = paste("Ano: ",ANO, 
                            "<br>",
                            "Faixa Etaria :", as.factor(idade_cat))), 
           position = "stack", show.legend = TRUE, color = 'black', 
           na.rm = TRUE, alpha = 0.5) +
  theme_gray() + 
  theme(legend.position='none') +
  ggtitle("") +
  ylab("") +
  xlab("") +
  scale_fill_brewer(direction=-1,palette = "Set2") -> idade_PA_21

ggplotly(idade_PA_21,
         height = 860, tooltip = c("count","text"))


#### FAZER CAUSA DE MORTE



# Taxas 19

t1_pa_f_19 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2019", IDADE <= "4", 
                                     Sexo == "F"))/ proj_PA_M[2,1] ; t1_pa_f_19 
t2_pa_f_19 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2019", idade_cat == "5-9", 
                                     Sexo == "F"))/ proj_PA_M[3,1] ; t2_pa_f_19 
t3_pa_f_19 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2019", idade_cat == "10-14", 
                                     Sexo == "F"))/ proj_PA_M[4,1] ; t3_pa_f_19 
t4_pa_f_19 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2019", idade_cat == "15-19", 
                                     Sexo == "F"))/ proj_PA_M[5,1] ; t4_pa_f_19 
t5_pa_f_19 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2019", idade_cat == "20-24", 
                                     Sexo == "F"))/ proj_PA_M[6,1] ; t5_pa_f_19 
t6_pa_f_19 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2019", idade_cat == "25-29", 
                                     Sexo == "F"))/ proj_PA_M[7,1] ; t6_pa_f_19 
t7_pa_f_19 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2019", idade_cat == "30-34", 
                                     Sexo == "F"))/ proj_PA_M[8,1] ; t7_pa_f_19 
t8_pa_f_19 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2019", idade_cat == "35-39", 
                                     Sexo == "F"))/ proj_PA_M[9,1] ; t8_pa_f_19 
t9_pa_f_19 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2019", idade_cat == "40-44", 
                                     Sexo == "F"))/ proj_PA_M[10,1] ; t9_pa_f_19 
t10_pa_f_19 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2019", idade_cat == "45-49", 
                                      Sexo == "F"))/ proj_PA_M[11,1] ; t10_pa_f_19 
t11_pa_f_19 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2019", idade_cat == "50-54", 
                                      Sexo == "F"))/ proj_PA_M[12,1] ; t11_pa_f_19 
t12_pa_f_19 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2019", idade_cat == "55-59", 
                                      Sexo == "F"))/ proj_PA_M[13,1] ; t12_pa_f_19 
t13_pa_f_19 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2019", idade_cat == "60-64", 
                                      Sexo == "F"))/ proj_PA_M[14,1] ; t13_pa_f_19 
t14_pa_f_19 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2019", idade_cat == "65-69", 
                                      Sexo == "F"))/ proj_PA_M[15,1] ; t14_pa_f_19 
t15_pa_f_19 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2019", idade_cat == "70-74", 
                                      Sexo == "F"))/ proj_PA_M[16,1] ; t15_pa_f_19 
t16_pa_f_19 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2019", idade_cat == "75-79", 
                                      Sexo == "F"))/ proj_PA_M[17,1] ; t16_pa_f_19 
t17_pa_f_19 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2019", idade_cat == "80-84", 
                                      Sexo == "F"))/ proj_PA_M[18,1] ; t17_pa_f_19 
t18_pa_f_19 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2019", idade_cat == "85-89", 
                                      Sexo == "F"))/ proj_PA_M[19,1] ; t18_pa_f_19 
t19_pa_f_19 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2019", IDADE >= "90", 
                                      Sexo == "F"))/ proj_PA_M[20,1] ; t19_pa_f_19 


t1_pa_h_19 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2019", IDADE <= "4", 
                                     Sexo == "M"))/ proj_PA_H[2,1] ; t1_pa_h_19 
t2_pa_h_19 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2019", idade_cat == "5-9", 
                                     Sexo == "M"))/ proj_PA_H[3,1] ; t2_pa_h_19 
t3_pa_h_19 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2019", idade_cat == "10-14", 
                                     Sexo == "M"))/ proj_PA_H[4,1] ; t3_pa_h_19 
t4_pa_h_19 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2019", idade_cat == "15-19", 
                                     Sexo == "M"))/ proj_PA_H[5,1] ; t4_pa_h_19 
t5_pa_h_19 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2019", idade_cat == "20-24", 
                                     Sexo == "M"))/ proj_PA_H[6,1] ; t5_pa_h_19 
t6_pa_h_19 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2019", idade_cat == "25-29", 
                                     Sexo == "M"))/ proj_PA_H[7,1] ; t6_pa_h_19 
t7_pa_h_19 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2019", idade_cat == "30-34", 
                                     Sexo == "M"))/ proj_PA_H[8,1] ; t7_pa_h_19 
t8_pa_h_19 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2019", idade_cat == "35-39", 
                                     Sexo == "M"))/ proj_PA_H[9,1] ; t8_pa_h_19 
t9_pa_h_19 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2019", idade_cat == "40-44", 
                                     Sexo == "M"))/ proj_PA_H[10,1] ; t9_pa_h_19 
t10_pa_h_19 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2019", idade_cat == "45-49", 
                                      Sexo == "M"))/ proj_PA_H[11,1] ; t10_pa_h_19 
t11_pa_h_19 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2019", idade_cat == "50-54", 
                                      Sexo == "M"))/ proj_PA_H[12,1] ; t11_pa_h_19 
t12_pa_h_19 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2019", idade_cat == "55-59", 
                                      Sexo == "M"))/ proj_PA_H[13,1] ; t12_pa_f_19 
t13_pa_h_19 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2019", idade_cat == "60-64", 
                                      Sexo == "M"))/ proj_PA_H[14,1] ; t13_pa_f_19 
t14_pa_h_19 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2019", idade_cat == "65-69", 
                                      Sexo == "M"))/ proj_PA_H[15,1] ; t14_pa_f_19 
t15_pa_h_19 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2019", idade_cat == "70-74", 
                                      Sexo == "M"))/ proj_PA_H[16,1] ; t15_pa_f_19 
t16_pa_h_19 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2019", idade_cat == "75-79", 
                                      Sexo == "M"))/ proj_PA_H[17,1] ; t16_pa_f_19 
t17_pa_h_19 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2019", idade_cat == "80-84", 
                                      Sexo == "M"))/ proj_PA_H[18,1] ; t17_pa_f_19 
t18_pa_h_19 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2019", idade_cat == "85-89", 
                                      Sexo == "M"))/ proj_PA_H[19,1] ; t18_pa_f_19 
t19_pa_h_19 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2019", IDADE >= "90", 
                                      Sexo == "M"))/ proj_PA_H[20,1] ; t19_pa_f_19 


# Taxas 21

t1_pa_f_21 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2021", IDADE <= "4", 
                                     Sexo == "F"))/ proj_PA_M[2,3] ; t1_pa_f_19 
t2_pa_f_21 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2021", idade_cat == "5-9", 
                                     Sexo == "F"))/ proj_PA_M[3,3] ; t2_pa_f_19 
t3_pa_f_21 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2021", idade_cat == "10-14", 
                                     Sexo == "F"))/ proj_PA_M[4,3] ; t3_pa_f_19 
t4_pa_f_21 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2021", idade_cat == "15-19", 
                                     Sexo == "F"))/ proj_PA_M[5,3] ; t4_pa_f_19 
t5_pa_f_21 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2021", idade_cat == "20-24", 
                                     Sexo == "F"))/ proj_PA_M[6,3] ; t5_pa_f_19 
t6_pa_f_21 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2021", idade_cat == "25-29", 
                                     Sexo == "F"))/ proj_PA_M[7,3] ; t6_pa_f_19 
t7_pa_f_21 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2021", idade_cat == "30-34", 
                                     Sexo == "F"))/ proj_PA_M[8,3] ; t7_pa_f_19 
t8_pa_f_21 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2021", idade_cat == "35-39", 
                                     Sexo == "F"))/ proj_PA_M[9,3] ; t8_pa_f_19 
t9_pa_f_21 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2021", idade_cat == "40-44", 
                                     Sexo == "F"))/ proj_PA_M[10,3] ; t9_pa_f_19 
t10_pa_f_21 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2021", idade_cat == "45-49", 
                                      Sexo == "F"))/ proj_PA_M[11,3] ; t10_pa_f_19 
t11_pa_f_21 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2021", idade_cat == "50-54", 
                                      Sexo == "F"))/ proj_PA_M[12,3] ; t11_pa_f_19 
t12_pa_f_21 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2021", idade_cat == "55-59", 
                                      Sexo == "F"))/ proj_PA_M[13,3] ; t12_pa_f_19 
t13_pa_f_21 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2021", idade_cat == "60-64", 
                                      Sexo == "F"))/ proj_PA_M[14,3] ; t13_pa_f_19 
t14_pa_f_21 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2021", idade_cat == "65-69", 
                                      Sexo == "F"))/ proj_PA_M[15,3] ; t14_pa_f_19 
t15_pa_f_21 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2021", idade_cat == "70-74", 
                                      Sexo == "F"))/ proj_PA_M[16,3] ; t15_pa_f_19 
t16_pa_f_21 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2021", idade_cat == "75-79", 
                                      Sexo == "F"))/ proj_PA_M[17,3] ; t16_pa_f_19 
t17_pa_f_21 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2021", idade_cat == "80-84", 
                                      Sexo == "F"))/ proj_PA_M[18,3] ; t17_pa_f_19 
t18_pa_f_21 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2021", idade_cat == "85-89", 
                                      Sexo == "F"))/ proj_PA_M[19,3] ; t18_pa_f_19 
t19_pa_f_21 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2021", IDADE >= "90", 
                                      Sexo == "F"))/ proj_PA_M[20,3] ; t19_pa_f_19 


t1_pa_h_21 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2021", IDADE <= "4", 
                                     Sexo == "M"))/ proj_PA_H[2,3] ; t1_pa_h_19 
t2_pa_h_21 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2021", idade_cat == "5-9", 
                                     Sexo == "M"))/ proj_PA_H[3,3] ; t2_pa_h_19 
t3_pa_h_21 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2021", idade_cat == "10-14", 
                                     Sexo == "M"))/ proj_PA_H[4,3] ; t3_pa_h_19 
t4_pa_h_21 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2021", idade_cat == "15-19", 
                                     Sexo == "M"))/ proj_PA_H[5,3] ; t4_pa_h_19 
t5_pa_h_21 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2021", idade_cat == "20-24", 
                                     Sexo == "M"))/ proj_PA_H[6,3] ; t5_pa_h_19 
t6_pa_h_21 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2021", idade_cat == "25-29", 
                                     Sexo == "M"))/ proj_PA_H[7,3] ; t6_pa_h_19 
t7_pa_h_21 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2021", idade_cat == "30-34", 
                                     Sexo == "M"))/ proj_PA_H[8,3] ; t7_pa_h_19 
t8_pa_h_21 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2021", idade_cat == "35-39", 
                                     Sexo == "M"))/ proj_PA_H[9,3] ; t8_pa_h_19 
t9_pa_h_21 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2021", idade_cat == "40-44", 
                                     Sexo == "M"))/ proj_PA_H[10,3] ; t9_pa_h_19 
t10_pa_h_21 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2021", idade_cat == "45-49", 
                                      Sexo == "M"))/ proj_PA_H[11,3] ; t10_pa_h_19 
t11_pa_h_21 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2021", idade_cat == "50-54", 
                                      Sexo == "M"))/ proj_PA_H[12,3] ; t11_pa_h_19 
t12_pa_h_21 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2021", idade_cat == "55-59", 
                                      Sexo == "M"))/ proj_PA_H[13,3] ; t12_pa_f_19 
t13_pa_h_21 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2021", idade_cat == "60-64", 
                                      Sexo == "M"))/ proj_PA_H[14,3] ; t13_pa_f_19 
t14_pa_h_21 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2021", idade_cat == "65-69", 
                                      Sexo == "M"))/ proj_PA_H[15,3] ; t14_pa_f_19 
t15_pa_h_21 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2021", idade_cat == "70-74", 
                                      Sexo == "M"))/ proj_PA_H[16,3] ; t15_pa_f_19 
t16_pa_h_21 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2021", idade_cat == "75-79", 
                                      Sexo == "M"))/ proj_PA_H[17,3] ; t16_pa_f_19 
t17_pa_h_21 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2021", idade_cat == "80-84", 
                                      Sexo == "M"))/ proj_PA_H[18,3] ; t17_pa_f_19 
t18_pa_h_21 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2021", idade_cat == "85-89", 
                                      Sexo == "M"))/ proj_PA_H[19,3] ; t18_pa_f_19 
t19_pa_h_21 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2021", IDADE >= "90", 
                                      Sexo == "M"))/ proj_PA_H[20,3] ; t19_pa_f_19 


# Taxas 20

t1_pa_f_20 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2020", IDADE <= "4", 
                                     Sexo == "F"))/ proj_PA_M[2,2] ; t1_pa_f_19 
t2_pa_f_20 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2020", idade_cat == "5-9", 
                                     Sexo == "F"))/ proj_PA_M[3,2] ; t2_pa_f_19 
t3_pa_f_20 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2020", idade_cat == "10-14", 
                                     Sexo == "F"))/ proj_PA_M[4,2] ; t3_pa_f_19 
t4_pa_f_20 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2020", idade_cat == "15-19", 
                                     Sexo == "F"))/ proj_PA_M[5,2] ; t4_pa_f_19 
t5_pa_f_20 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2020", idade_cat == "20-24", 
                                     Sexo == "F"))/ proj_PA_M[6,2] ; t5_pa_f_19 
t6_pa_f_20 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2020", idade_cat == "25-29", 
                                     Sexo == "F"))/ proj_PA_M[7,2] ; t6_pa_f_19 
t7_pa_f_20 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2020", idade_cat == "30-34", 
                                     Sexo == "F"))/ proj_PA_M[8,2] ; t7_pa_f_19 
t8_pa_f_20 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2020", idade_cat == "35-39", 
                                     Sexo == "F"))/ proj_PA_M[9,2] ; t8_pa_f_19 
t9_pa_f_20 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2020", idade_cat == "40-44", 
                                     Sexo == "F"))/ proj_PA_M[10,2] ; t9_pa_f_19 
t10_pa_f_20 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2020", idade_cat == "45-49", 
                                      Sexo == "F"))/ proj_PA_M[11,2] ; t10_pa_f_19 
t11_pa_f_20 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2020", idade_cat == "50-54", 
                                      Sexo == "F"))/ proj_PA_M[12,2] ; t11_pa_f_19 
t12_pa_f_20 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2020", idade_cat == "55-59", 
                                      Sexo == "F"))/ proj_PA_M[13,2] ; t12_pa_f_19 
t13_pa_f_20 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2020", idade_cat == "60-64", 
                                      Sexo == "F"))/ proj_PA_M[14,2] ; t13_pa_f_19 
t14_pa_f_20 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2020", idade_cat == "65-69", 
                                      Sexo == "F"))/ proj_PA_M[15,2] ; t14_pa_f_19 
t15_pa_f_20 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2020", idade_cat == "70-74", 
                                      Sexo == "F"))/ proj_PA_M[16,2] ; t15_pa_f_19 
t16_pa_f_20 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2020", idade_cat == "75-79", 
                                      Sexo == "F"))/ proj_PA_M[17,2] ; t16_pa_f_19 
t17_pa_f_20 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2020", idade_cat == "80-84", 
                                      Sexo == "F"))/ proj_PA_M[18,2] ; t17_pa_f_19 
t18_pa_f_20 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2020", idade_cat == "85-89", 
                                      Sexo == "F"))/ proj_PA_M[19,2] ; t18_pa_f_19 
t19_pa_f_20 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2020", IDADE >= "90", 
                                      Sexo == "F"))/ proj_PA_M[20,2] ; t19_pa_f_19 


t1_pa_h_20 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2020", IDADE <= "4", 
                                     Sexo == "M"))/ proj_PA_H[2,2] ; t1_pa_h_19 
t2_pa_h_20 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2020", idade_cat == "5-9", 
                                     Sexo == "M"))/ proj_PA_H[3,2] ; t2_pa_h_19 
t3_pa_h_20 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2020", idade_cat == "10-14", 
                                     Sexo == "M"))/ proj_PA_H[4,2] ; t3_pa_h_19 
t4_pa_h_20 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2020", idade_cat == "15-19", 
                                     Sexo == "M"))/ proj_PA_H[5,2] ; t4_pa_h_19 
t5_pa_h_20 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2020", idade_cat == "20-24", 
                                     Sexo == "M"))/ proj_PA_H[6,2] ; t5_pa_h_19 
t6_pa_h_20 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2020", idade_cat == "25-29", 
                                     Sexo == "M"))/ proj_PA_H[7,2] ; t6_pa_h_19 
t7_pa_h_20 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2020", idade_cat == "30-34", 
                                     Sexo == "M"))/ proj_PA_H[8,2] ; t7_pa_h_19 
t8_pa_h_20 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2020", idade_cat == "35-39", 
                                     Sexo == "M"))/ proj_PA_H[9,2] ; t8_pa_h_19 
t9_pa_h_20 <- 100000 * nrow(DOPA %>% 
                              filter(ANO == "2020", idade_cat == "40-44", 
                                     Sexo == "M"))/ proj_PA_H[10,2] ; t9_pa_h_19 
t10_pa_h_20 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2020", idade_cat == "45-49", 
                                      Sexo == "M"))/ proj_PA_H[11,2] ; t10_pa_h_19 
t11_pa_h_20 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2020", idade_cat == "50-54", 
                                      Sexo == "M"))/ proj_PA_H[12,2] ; t11_pa_h_19 
t12_pa_h_20 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2020", idade_cat == "55-59", 
                                      Sexo == "M"))/ proj_PA_H[13,2] ; t12_pa_f_19 
t13_pa_h_20 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2020", idade_cat == "60-64", 
                                      Sexo == "M"))/ proj_PA_H[14,2] ; t13_pa_f_19 
t14_pa_h_20 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2020", idade_cat == "65-69", 
                                      Sexo == "M"))/ proj_PA_H[15,2] ; t14_pa_f_19 
t15_pa_h_20 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2020", idade_cat == "70-74", 
                                      Sexo == "M"))/ proj_PA_H[16,2] ; t15_pa_f_19 
t16_pa_h_20 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2020", idade_cat == "75-79", 
                                      Sexo == "M"))/ proj_PA_H[17,2] ; t16_pa_f_19 
t17_pa_h_20 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2020", idade_cat == "80-84", 
                                      Sexo == "M"))/ proj_PA_H[18,2] ; t17_pa_f_19 
t18_pa_h_20 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2020", idade_cat == "85-89", 
                                      Sexo == "M"))/ proj_PA_H[19,2] ; t18_pa_f_19 
t19_pa_h_20 <- 100000 * nrow(DOPA %>% 
                               filter(ANO == "2020", IDADE >= "90", 
                                      Sexo == "M"))/ proj_PA_H[20,2] ; t19_pa_f_19 




################### Pernambuco ###################

# Sexo 

DOPE$ANO <- factor(as.character(DOPE$ANO), 
                   levels = c('2019','2020', '2021'))


ggplot(data = DOPE %>% filter(Sexo != "Ignorado"), 
       aes(x = reorder(ANO, ANO, function(x) -length(x)), 
           fill = Sexo, text = paste("Sexo: ",Sexo, "<br>",
                                     " Ano:", as.factor(ANO)))) +
  geom_bar(position = "dodge", show.legend = TRUE, color = 'black', 
           na.rm = TRUE, alpha = 0.5, width = 0.8) +
  theme_gray() + 
  ggtitle("")+
  theme(legend.position='none') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())+
  scale_x_discrete(na.translate = FALSE)+
  scale_fill_brewer(palette = "Set1") -> sexo_PE

ggplotly(sexo_PE, tooltip = c("count","text"))


# Idade conjunto

ggplot(data = DOPE %>% filter(is.na(idade_cat) == FALSE)) +
  geom_bar(aes(y = idade_cat, fill = ANO,
               text = paste("Ano: ",ANO, 
                            "<br>",
                            "Faixa Etaria :", as.factor(idade_cat))), 
           position = "stack", show.legend = TRUE, color = 'black', 
           na.rm = TRUE, alpha = 0.5) +
  theme_gray() + 
  theme(legend.position='none') +
  ggtitle("") +
  ylab("") +
  xlab("") +
  scale_fill_brewer(direction=-1,palette = "Set2") -> idade_PE

ggplotly(idade_PE,
         height = 860, tooltip = c("count","text"))


# Idade 2019

ggplot(data = DOPE %>% filter(is.na(idade_cat) == FALSE,
                              ANO == '2019')) +
  geom_bar(aes(y = idade_cat, fill = ANO,
               text = paste("Ano: ",ANO, 
                            "<br>",
                            "Faixa Etaria :", as.factor(idade_cat))), 
           position = "stack", show.legend = TRUE, color = 'black', 
           na.rm = TRUE, alpha = 0.5) +
  theme_gray() + 
  theme(legend.position='none') +
  ggtitle("") +
  ylab("") +
  xlab("") +
  scale_fill_brewer(direction=-1,palette = "Set2") -> idade_PE_19

ggplotly(idade_PE_19,
         height = 860, tooltip = c("count","text"))


# Idade 2020

ggplot(data = DOPE %>% filter(is.na(idade_cat) == FALSE,
                              ANO == '2020')) +
  geom_bar(aes(y = idade_cat, fill = ANO,
               text = paste("Ano: ",ANO, 
                            "<br>",
                            "Faixa Etaria :", as.factor(idade_cat))), 
           position = "stack", show.legend = TRUE, color = 'black', 
           na.rm = TRUE, alpha = 0.5) +
  theme_gray() + 
  theme(legend.position='none') +
  ggtitle("") +
  ylab("") +
  xlab("") +
  scale_fill_brewer(direction=-1,palette = "Set2") -> idade_PE_20

ggplotly(idade_PE_20,
         height = 860, tooltip = c("count","text"))


# Idade 2021

ggplot(data = DOPE %>% filter(is.na(idade_cat) == FALSE,
                              ANO == '2021')) +
  geom_bar(aes(y = idade_cat, fill = ANO,
               text = paste("Ano: ",ANO, 
                            "<br>",
                            "Faixa Etaria :", as.factor(idade_cat))), 
           position = "stack", show.legend = TRUE, color = 'black', 
           na.rm = TRUE, alpha = 0.5) +
  theme_gray() + 
  theme(legend.position='none') +
  ggtitle("") +
  ylab("") +
  xlab("") +
  scale_fill_brewer(direction=-1,palette = "Set2") -> idade_PE_21

ggplotly(idade_PE_21,
         height = 860, tooltip = c("count","text"))





# Taxas 19

t1_pe_f_19 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2019", IDADE <= "4", 
                                     Sexo == "F"))/ proj_PE_M[2,1] ; t1_pa_f_19 
t2_pe_f_19 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2019", idade_cat == "5-9", 
                                     Sexo == "F"))/ proj_PE_M[3,1] ; t2_pa_f_19 
t3_pe_f_19 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2019", idade_cat == "10-14", 
                                     Sexo == "F"))/ proj_PE_M[4,1] ; t3_pa_f_19 
t4_pe_f_19 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2019", idade_cat == "15-19", 
                                     Sexo == "F"))/ proj_PE_M[5,1] ; t4_pa_f_19 
t5_pe_f_19 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2019", idade_cat == "20-24", 
                                     Sexo == "F"))/ proj_PE_M[6,1] ; t5_pa_f_19 
t6_pe_f_19 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2019", idade_cat == "25-29", 
                                     Sexo == "F"))/ proj_PE_M[7,1] ; t6_pa_f_19 
t7_pe_f_19 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2019", idade_cat == "30-34", 
                                     Sexo == "F"))/ proj_PE_M[8,1] ; t7_pa_f_19 
t8_pe_f_19 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2019", idade_cat == "35-39", 
                                     Sexo == "F"))/ proj_PE_M[9,1] ; t8_pa_f_19 
t9_pe_f_19 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2019", idade_cat == "40-44", 
                                     Sexo == "F"))/ proj_PE_M[10,1] ; t9_pa_f_19 
t10_pe_f_19 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2019", idade_cat == "45-49", 
                                      Sexo == "F"))/ proj_PE_M[11,1] ; t10_pa_f_19 
t11_pe_f_19 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2019", idade_cat == "50-54", 
                                      Sexo == "F"))/ proj_PE_M[12,1] ; t11_pa_f_19 
t12_pe_f_19 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2019", idade_cat == "55-59", 
                                      Sexo == "F"))/ proj_PE_M[13,1] ; t12_pa_f_19 
t13_pe_f_19 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2019", idade_cat == "60-64", 
                                      Sexo == "F"))/ proj_PE_M[14,1] ; t13_pa_f_19 
t14_pe_f_19 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2019", idade_cat == "65-69", 
                                      Sexo == "F"))/ proj_PE_M[15,1] ; t14_pa_f_19 
t15_pe_f_19 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2019", idade_cat == "70-74", 
                                      Sexo == "F"))/ proj_PE_M[16,1] ; t15_pa_f_19 
t16_pe_f_19 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2019", idade_cat == "75-79", 
                                      Sexo == "F"))/ proj_PE_M[17,1] ; t16_pa_f_19 
t17_pe_f_19 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2019", idade_cat == "80-84", 
                                      Sexo == "F"))/ proj_PE_M[18,1] ; t17_pa_f_19 
t18_pe_f_19 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2019", idade_cat == "85-89", 
                                      Sexo == "F"))/ proj_PE_M[19,1] ; t18_pa_f_19 
t19_pe_f_19 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2019", IDADE >= "90", 
                                      Sexo == "F"))/ proj_PE_M[20,1] ; t19_pa_f_19 


t1_pe_h_19 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2019", IDADE <= "4", 
                                     Sexo == "M"))/ proj_PE_H[2,1] ; t1_pa_h_19 
t2_pe_h_19 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2019", idade_cat == "5-9", 
                                     Sexo == "M"))/ proj_PE_H[3,1] ; t2_pa_h_19 
t3_pe_h_19 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2019", idade_cat == "10-14", 
                                     Sexo == "M"))/ proj_PE_H[4,1] ; t3_pa_h_19 
t4_pe_h_19 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2019", idade_cat == "15-19", 
                                     Sexo == "M"))/ proj_PE_H[5,1] ; t4_pa_h_19 
t5_pe_h_19 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2019", idade_cat == "20-24", 
                                     Sexo == "M"))/ proj_PE_H[6,1] ; t5_pa_h_19 
t6_pe_h_19 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2019", idade_cat == "25-29", 
                                     Sexo == "M"))/ proj_PE_H[7,1] ; t6_pa_h_19 
t7_pe_h_19 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2019", idade_cat == "30-34", 
                                     Sexo == "M"))/ proj_PE_H[8,1] ; t7_pa_h_19 
t8_pe_h_19 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2019", idade_cat == "35-39", 
                                     Sexo == "M"))/ proj_PE_H[9,1] ; t8_pa_h_19 
t9_pe_h_19 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2019", idade_cat == "40-44", 
                                     Sexo == "M"))/ proj_PE_H[10,1] ; t9_pa_h_19 
t10_pe_h_19 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2019", idade_cat == "45-49", 
                                      Sexo == "M"))/ proj_PE_H[11,1] ; t10_pa_h_19 
t11_pe_h_19 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2019", idade_cat == "50-54", 
                                      Sexo == "M"))/ proj_PE_H[12,1] ; t11_pa_h_19 
t12_pe_h_19 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2019", idade_cat == "55-59", 
                                      Sexo == "M"))/ proj_PE_H[13,1] ; t12_pa_f_19 
t13_pe_h_19 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2019", idade_cat == "60-64", 
                                      Sexo == "M"))/ proj_PE_H[14,1] ; t13_pa_f_19 
t14_pe_h_19 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2019", idade_cat == "65-69", 
                                      Sexo == "M"))/ proj_PE_H[15,1] ; t14_pa_f_19 
t15_pe_h_19 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2019", idade_cat == "70-74", 
                                      Sexo == "M"))/ proj_PE_H[16,1] ; t15_pa_f_19 
t16_pe_h_19 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2019", idade_cat == "75-79", 
                                      Sexo == "M"))/ proj_PE_H[17,1] ; t16_pa_f_19 
t17_pe_h_19 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2019", idade_cat == "80-84", 
                                      Sexo == "M"))/ proj_PE_H[18,1] ; t17_pa_f_19 
t18_pe_h_19 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2019", idade_cat == "85-89", 
                                      Sexo == "M"))/ proj_PE_H[19,1] ; t18_pa_f_19 
t19_pe_h_19 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2019", IDADE >= "90", 
                                      Sexo == "M"))/ proj_PE_H[20,1] ; t19_pa_f_19 


# Taxas 21

t1_pe_f_21 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2021", IDADE <= "4", 
                                     Sexo == "F"))/ proj_PE_M[2,3] ; t1_pa_f_19 
t2_pe_f_21 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2021", idade_cat == "5-9", 
                                     Sexo == "F"))/ proj_PE_M[3,3] ; t2_pa_f_19 
t3_pe_f_21 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2021", idade_cat == "10-14", 
                                     Sexo == "F"))/ proj_PE_M[4,3] ; t3_pa_f_19 
t4_pe_f_21 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2021", idade_cat == "15-19", 
                                     Sexo == "F"))/ proj_PE_M[5,3] ; t4_pa_f_19 
t5_pe_f_21 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2021", idade_cat == "20-24", 
                                     Sexo == "F"))/ proj_PE_M[6,3] ; t5_pa_f_19 
t6_pe_f_21 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2021", idade_cat == "25-29", 
                                     Sexo == "F"))/ proj_PE_M[7,3] ; t6_pa_f_19 
t7_pe_f_21 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2021", idade_cat == "30-34", 
                                     Sexo == "F"))/ proj_PE_M[8,3] ; t7_pa_f_19 
t8_pe_f_21 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2021", idade_cat == "35-39", 
                                     Sexo == "F"))/ proj_PE_M[9,3] ; t8_pa_f_19 
t9_pe_f_21 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2021", idade_cat == "40-44", 
                                     Sexo == "F"))/ proj_PE_M[10,3] ; t9_pa_f_19 
t10_pe_f_21 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2021", idade_cat == "45-49", 
                                      Sexo == "F"))/ proj_PE_M[11,3] ; t10_pa_f_19 
t11_pe_f_21 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2021", idade_cat == "50-54", 
                                      Sexo == "F"))/ proj_PE_M[12,3] ; t11_pa_f_19 
t12_pe_f_21 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2021", idade_cat == "55-59", 
                                      Sexo == "F"))/ proj_PE_M[13,3] ; t12_pa_f_19 
t13_pe_f_21 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2021", idade_cat == "60-64", 
                                      Sexo == "F"))/ proj_PE_M[14,3] ; t13_pa_f_19 
t14_pe_f_21 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2021", idade_cat == "65-69", 
                                      Sexo == "F"))/ proj_PE_M[15,3] ; t14_pa_f_19 
t15_pe_f_21 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2021", idade_cat == "70-74", 
                                      Sexo == "F"))/ proj_PE_M[16,3] ; t15_pa_f_19 
t16_pe_f_21 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2021", idade_cat == "75-79", 
                                      Sexo == "F"))/ proj_PE_M[17,3] ; t16_pa_f_19 
t17_pe_f_21 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2021", idade_cat == "80-84", 
                                      Sexo == "F"))/ proj_PE_M[18,3] ; t17_pa_f_19 
t18_pe_f_21 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2021", idade_cat == "85-89", 
                                      Sexo == "F"))/ proj_PE_M[19,3] ; t18_pa_f_19 
t19_pe_f_21 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2021", IDADE >= "90", 
                                      Sexo == "F"))/ proj_PE_M[20,3] ; t19_pa_f_19 


t1_pe_h_21 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2021", IDADE <= "4", 
                                     Sexo == "M"))/ proj_PE_H[2,3] ; t1_pa_h_19 
t2_pe_h_21 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2021", idade_cat == "5-9", 
                                     Sexo == "M"))/ proj_PE_H[3,3] ; t2_pa_h_19 
t3_pe_h_21 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2021", idade_cat == "10-14", 
                                     Sexo == "M"))/ proj_PE_H[4,3] ; t3_pa_h_19 
t4_pe_h_21 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2021", idade_cat == "15-19", 
                                     Sexo == "M"))/ proj_PE_H[5,3] ; t4_pa_h_19 
t5_pe_h_21 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2021", idade_cat == "20-24", 
                                     Sexo == "M"))/ proj_PE_H[6,3] ; t5_pa_h_19 
t6_pe_h_21 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2021", idade_cat == "25-29", 
                                     Sexo == "M"))/ proj_PE_H[7,3] ; t6_pa_h_19 
t7_pe_h_21 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2021", idade_cat == "30-34", 
                                     Sexo == "M"))/ proj_PE_H[8,3] ; t7_pa_h_19 
t8_pe_h_21 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2021", idade_cat == "35-39", 
                                     Sexo == "M"))/ proj_PE_H[9,3] ; t8_pa_h_19 
t9_pe_h_21 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2021", idade_cat == "40-44", 
                                     Sexo == "M"))/ proj_PE_H[10,3] ; t9_pa_h_19 
t10_pe_h_21 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2021", idade_cat == "45-49", 
                                      Sexo == "M"))/ proj_PE_H[11,3] ; t10_pa_h_19 
t11_pe_h_21 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2021", idade_cat == "50-54", 
                                      Sexo == "M"))/ proj_PE_H[12,3] ; t11_pa_h_19 
t12_pe_h_21 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2021", idade_cat == "55-59", 
                                      Sexo == "M"))/ proj_PE_H[13,3] ; t12_pa_f_19 
t13_pe_h_21 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2021", idade_cat == "60-64", 
                                      Sexo == "M"))/ proj_PE_H[14,3] ; t13_pa_f_19 
t14_pe_h_21 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2021", idade_cat == "65-69", 
                                      Sexo == "M"))/ proj_PE_H[15,3] ; t14_pa_f_19 
t15_pe_h_21 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2021", idade_cat == "70-74", 
                                      Sexo == "M"))/ proj_PE_H[16,3] ; t15_pa_f_19 
t16_pe_h_21 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2021", idade_cat == "75-79", 
                                      Sexo == "M"))/ proj_PE_H[17,3] ; t16_pa_f_19 
t17_pe_h_21 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2021", idade_cat == "80-84", 
                                      Sexo == "M"))/ proj_PE_H[18,3] ; t17_pa_f_19 
t18_pe_h_21 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2021", idade_cat == "85-89", 
                                      Sexo == "M"))/ proj_PE_H[19,3] ; t18_pa_f_19 
t19_pe_h_21 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2021", IDADE >= "90", 
                                      Sexo == "M"))/ proj_PE_H[20,3] ; t19_pa_f_19 


# Taxas 20

t1_pe_f_20 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2020", IDADE <= "4", 
                                     Sexo == "F"))/ proj_PE_M[2,2] ; t1_pa_f_19 
t2_pe_f_20 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2020", idade_cat == "5-9", 
                                     Sexo == "F"))/ proj_PE_M[3,2] ; t2_pa_f_19 
t3_pe_f_20 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2020", idade_cat == "10-14", 
                                     Sexo == "F"))/ proj_PE_M[4,2] ; t3_pa_f_19 
t4_pe_f_20 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2020", idade_cat == "15-19", 
                                     Sexo == "F"))/ proj_PE_M[5,2] ; t4_pa_f_19 
t5_pe_f_20 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2020", idade_cat == "20-24", 
                                     Sexo == "F"))/ proj_PE_M[6,2] ; t5_pa_f_19 
t6_pe_f_20 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2020", idade_cat == "25-29", 
                                     Sexo == "F"))/ proj_PE_M[7,2] ; t6_pa_f_19 
t7_pe_f_20 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2020", idade_cat == "30-34", 
                                     Sexo == "F"))/ proj_PE_M[8,2] ; t7_pa_f_19 
t8_pe_f_20 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2020", idade_cat == "35-39", 
                                     Sexo == "F"))/ proj_PE_M[9,2] ; t8_pa_f_19 
t9_pe_f_20 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2020", idade_cat == "40-44", 
                                     Sexo == "F"))/ proj_PE_M[10,2] ; t9_pa_f_19 
t10_pe_f_20 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2020", idade_cat == "45-49", 
                                      Sexo == "F"))/ proj_PE_M[11,2] ; t10_pa_f_19 
t11_pe_f_20 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2020", idade_cat == "50-54", 
                                      Sexo == "F"))/ proj_PE_M[12,2] ; t11_pa_f_19 
t12_pe_f_20 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2020", idade_cat == "55-59", 
                                      Sexo == "F"))/ proj_PE_M[13,2] ; t12_pa_f_19 
t13_pe_f_20 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2020", idade_cat == "60-64", 
                                      Sexo == "F"))/ proj_PE_M[14,2] ; t13_pa_f_19 
t14_pe_f_20 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2020", idade_cat == "65-69", 
                                      Sexo == "F"))/ proj_PE_M[15,2] ; t14_pa_f_19 
t15_pe_f_20 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2020", idade_cat == "70-74", 
                                      Sexo == "F"))/ proj_PE_M[16,2] ; t15_pa_f_19 
t16_pe_f_20 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2020", idade_cat == "75-79", 
                                      Sexo == "F"))/ proj_PE_M[17,2] ; t16_pa_f_19 
t17_pe_f_20 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2020", idade_cat == "80-84", 
                                      Sexo == "F"))/ proj_PE_M[18,2] ; t17_pa_f_19 
t18_pe_f_20 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2020", idade_cat == "85-89", 
                                      Sexo == "F"))/ proj_PE_M[19,2] ; t18_pa_f_19 
t19_pe_f_20 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2020", IDADE >= "90", 
                                      Sexo == "F"))/ proj_PE_M[20,2] ; t19_pa_f_19 


t1_pe_h_20 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2020", IDADE <= "4", 
                                     Sexo == "M"))/ proj_PE_H[2,2] ; t1_pa_h_19 
t2_pe_h_20 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2020", idade_cat == "5-9", 
                                     Sexo == "M"))/ proj_PE_H[3,2] ; t2_pa_h_19 
t3_pe_h_20 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2020", idade_cat == "10-14", 
                                     Sexo == "M"))/ proj_PE_H[4,2] ; t3_pa_h_19 
t4_pe_h_20 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2020", idade_cat == "15-19", 
                                     Sexo == "M"))/ proj_PE_H[5,2] ; t4_pa_h_19 
t5_pe_h_20 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2020", idade_cat == "20-24", 
                                     Sexo == "M"))/ proj_PE_H[6,2] ; t5_pa_h_19 
t6_pe_h_20 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2020", idade_cat == "25-29", 
                                     Sexo == "M"))/ proj_PE_H[7,2] ; t6_pa_h_19 
t7_pe_h_20 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2020", idade_cat == "30-34", 
                                     Sexo == "M"))/ proj_PE_H[8,2] ; t7_pa_h_19 
t8_pe_h_20 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2020", idade_cat == "35-39", 
                                     Sexo == "M"))/ proj_PE_H[9,2] ; t8_pa_h_19 
t9_pe_h_20 <- 100000 * nrow(DOPE %>% 
                              filter(ANO == "2020", idade_cat == "40-44", 
                                     Sexo == "M"))/ proj_PE_H[10,2] ; t9_pa_h_19 
t10_pe_h_20 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2020", idade_cat == "45-49", 
                                      Sexo == "M"))/ proj_PE_H[11,2] ; t10_pa_h_19 
t11_pe_h_20 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2020", idade_cat == "50-54", 
                                      Sexo == "M"))/ proj_PE_H[12,2] ; t11_pa_h_19 
t12_pe_h_20 <- 100000 * nrow(DOPE %>%
                               filter(ANO == "2020", idade_cat == "55-59", 
                                      Sexo == "M"))/ proj_PE_H[13,2] ; t12_pa_f_19 
t13_pe_h_20 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2020", idade_cat == "60-64", 
                                      Sexo == "M"))/ proj_PE_H[14,2] ; t13_pa_f_19 
t14_pe_h_20 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2020", idade_cat == "65-69", 
                                      Sexo == "M"))/ proj_PE_H[15,2] ; t14_pa_f_19 
t15_pe_h_20 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2020", idade_cat == "70-74", 
                                      Sexo == "M"))/ proj_PE_H[16,2] ; t15_pa_f_19 
t16_pe_h_20 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2020", idade_cat == "75-79", 
                                      Sexo == "M"))/ proj_PE_H[17,2] ; t16_pa_f_19 
t17_pe_h_20 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2020", idade_cat == "80-84", 
                                      Sexo == "M"))/ proj_PE_H[18,2] ; t17_pa_f_19 
t18_pe_h_20 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2020", idade_cat == "85-89", 
                                      Sexo == "M"))/ proj_PE_H[19,2] ; t18_pa_f_19 
t19_pe_h_20 <- 100000 * nrow(DOPE %>% 
                               filter(ANO == "2020", IDADE >= "90", 
                                      Sexo == "M"))/ proj_PE_H[20,2] ; t19_pa_f_19 
