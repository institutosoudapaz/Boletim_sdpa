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
  


###Passo 03: Criando os tabelas----
# Tabelas de Grandes Regiões
hd_vitima <- base_crimes %>% 
  filter(cod_reg==10|cod_reg==20|cod_reg==30) %>% 
  group_by(cod_reg,ano, semestre) %>% 
  summarise(sum(t50))

lat_vitima <- base_crimes %>% 
  filter(cod_reg==10|cod_reg==20|cod_reg==30) %>% 
  group_by(cod_reg,ano, semestre) %>% 
  summarise(sum(t49))

tot_estupro <- base_crimes %>% 
  filter(cod_reg==10|cod_reg==20|cod_reg==30) %>% 
  group_by(cod_reg,ano, semestre) %>% 
  summarise(sum(t201))

estupro_vuln <- base_crimes %>% 
  filter(cod_reg==10|cod_reg==20|cod_reg==30) %>% 
  group_by(cod_reg,ano, semestre) %>% 
  summarise(sum(t202))

roubo_outros <- base_crimes %>% 
  filter(cod_reg==10|cod_reg==20|cod_reg==30) %>% 
  group_by(cod_reg,ano, semestre) %>% 
  summarise(sum(t77))

RV <- base_crimes %>% 
  filter(cod_reg==10|cod_reg==20|cod_reg==30) %>% 
  group_by(cod_reg,ano, semestre) %>% 
  summarise(sum(t80))

ap_armas <- base_crimes %>% 
  filter(cod_reg==10|cod_reg==20|cod_reg==30) %>% 
  group_by(cod_reg,ano, semestre) %>% 
  summarise(sum(t1))

prisoes <- base_crimes %>% 
  filter(cod_reg==10|cod_reg==20|cod_reg==30) %>% 
  group_by(cod_reg,ano, semestre) %>% 
  summarise(sum(t75))

# Tabelas de DEINTER
hd_vitima_Deinter <- base_crimes %>% 
  filter(cod_reg > 30) %>% 
  group_by(cod_reg,ano, semestre) %>% 
  summarise(sum(t50))

lat_vitima_Deinter <- base_crimes %>% 
  filter(cod_reg > 30) %>% 
  group_by(cod_reg,ano, semestre) %>% 
  summarise(sum(t49))

tot_estupro_Deinter <- base_crimes %>% 
  filter(cod_reg > 30) %>% 
  group_by(cod_reg,ano, semestre) %>% 
  summarise(sum(t201))

estupro_vuln_Deinter <- base_crimes %>% 
  filter(cod_reg > 30) %>% 
  group_by(cod_reg,ano, semestre) %>% 
  summarise(sum(t202))

roubo_outros_Deinter <- base_crimes %>% 
  filter(cod_reg > 30) %>% 
  group_by(cod_reg,ano, semestre) %>% 
  summarise(sum(t77))

RV_Deinter <- base_crimes %>% 
  filter(cod_reg > 30) %>% 
  group_by(cod_reg,ano, semestre) %>% 
  summarise(sum(t80))

ap_armas_Deinter <- base_crimes %>% 
  filter(cod_reg > 30) %>% 
  group_by(cod_reg,ano, semestre) %>% 
  summarise(sum(t1))

prisoes_Deinter <- base_crimes %>% 
  filter(cod_reg > 30) %>% 
  group_by(cod_reg,ano, semestre) %>% 
  summarise(sum(t75))