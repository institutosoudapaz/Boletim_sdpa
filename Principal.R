#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Código para consolidar o Boletim Sou da Paz Analisa     #
# Elaborado por Rafael Rocha e Leonardo de Carvalho       #
# Data de início de desenvolvimento: 13/07/2021           #

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

###Passo 00: Carregar os pacotes ----
library(tidyverse)
library(dplyr)
library(ggplot2)

###Passo 01: Baixar os dados trimestrais da SSP e consolidar a base_trimestral: ----
  #Aqui vale pensar em rever o código para torna-lo mais eficiente, não vamos baixar tudo, 
  #só o período mais recente e empilhar com  a base que ja temos

###Passo 02: selecionar o ano e os demais marcos de tempo ----

base_trimestral <- readRDS("~\\Boletim_sdpa/data-raw/base_trimestral.RDS")
ano_referencia <- 2021
base_trimestral <- base_trimestral %>% 
  mutate(
    semestre = case_when(
      tri<3 ~ 1,
      TRUE  ~  2)
  )
base_crimes <- base_trimestral %>% 
  filter(ano >(ano_referencia-3)) %>% 
  select(ano, semestre,cod_reg,t1,t15,t21,t23,t40,t45,t46,
         t49,t50,t75,t77,t80,t201,t202,t203)
  


###Passo 03: Criando as tabelas de crimes----

tab_crimes <- base_crimes %>% 
  group_by(cod_reg,ano, semestre) %>% 
  summarise(hd_vitima = sum(t50),
            lat_vitima =sum(t49),
            tot_estupro =sum(t201),
            estupro_vuln =sum(t202),
            roubo_outros =sum(t77),
            roubo_veic = sum(t80),
            ap_armas =sum(t1),
            prisoes =sum(t75)
            )



###Passo 04: Criando os tabelas de população----
base_pop <- readRDS("~\\Boletim_sdpa/data-raw/pop_munic.RDS")
base_pop <- rename(base_pop, ano = Ano)
base_pop <- base_pop %>% 
  filter(Ano>(ano_referencia-3)) %>% 
  group_by(deinter, ano) %>% 
  summarise(sum(Pop))

# Padronizando os códigos de Deinter e regiões, note que aqui nao tem Interior
base_pop <- base_pop %>% 
  mutate(
    cod_reg = case_when(
      deinter == "Capital" ~ 10,
      deinter == "Grande São Paulo" ~ 20,
      deinter == "São José dos Campos" ~ 31,
      deinter == "Campinas" ~ 32,
      deinter == "Ribeirão Preto" ~ 33,
      deinter == "Bauru" ~ 34,
      deinter == "São José do Rio Preto" ~ 35,
      deinter == "Santos" ~ 36,
      deinter == "Sorocaba" ~ 37,
      deinter == "Presidente Prudente" ~ 38,
      deinter == "Piracicaba" ~ 39,
      deinter == "Araçatuba" ~ 40)
  )

#### Passo 05: Juntando população e crimes ----

tab_crimes2 <- left_join(tab_crimes, base_pop, by = "cod_reg", "ano")
