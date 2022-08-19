
# Código para consolidar Análise Especial Boletim Sou da Paz Analisa     
# 1 Semestre de 2022
# Data de início de desenvolvimento: 18/08/2022           

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

###Passo 00: Carregar os pacotes ----
library(tidyverse)
#library(stringr)
#library(tidyr)


###Passo 01: Baixar os dados trimestrais da SSP e consolidar a base_trimestral: ----
  #Aqui vale pensar em rever o código para torna-lo mais eficiente, não vamos baixar tudo, 
  #Aqui o resultado precisa ser um arquivo chamado "base_trimestral.RDS"

ano_referencia <- 2022

###Passo 02: Abre e trata a base trimestral ----

#Abre a base trimestral e criar a variavel semestre
base_trimestral <- read.csv2("../Boletim_sdpa/data-raw/base_trimestral_v4.csv")
base_trimestral <- base_trimestral %>% 
  mutate(
    semestre = case_when(
      tri<3 ~ 1,
      TRUE  ~  2)
  ) #%>% 
  #filter(cod_reg !=30)

# base_trimestral <- base_trimestral %>% 
#   mutate_at(c(3:96), as.numeric)
# base_trimestral[is.na(base_trimestral)] <- 0


#Modelo 1 para tri01; 2 para Semestre; 3 para tri03 e 4 para Anual

modelo <- 2

# trata a base trimestral a partir do modelo selecionado
if (modelo == 1){
  base_crimes <- base_trimestral %>% 
    filter(ano >(ano_referencia-2)) %>%
    mutate(periodo = paste(ano,"/", tri, "º Trimestre", sep = "")) %>% 
    select(periodo,tri,cod_reg,t01,t15,t21,t23,t40,t45,t46,
           t49,t50,t75,t77,t80,t201,t202,t203)
} else if (modelo == 2){
  base_crimes <- base_trimestral %>% 
    filter(ano >(ano_referencia-5)) %>% #alterado para incluir ano de 2018
    mutate(periodo = paste(ano,"/", semestre, "º Semestre", sep = "")) %>% 
    select(periodo,tri,cod_reg,t01,t15,t21,t23,t40,t45,t46,
           t49,t50,t75,t77, t78, t79, t80, t81,t201,t202,t203)
} else if (modelo == 3){
    base_crimes <- base_trimestral %>% 
      filter(ano >(ano_referencia-2)) %>%
      mutate(periodo = paste(ano,"/", tri, "º Trimestre", sep = "")) %>% 
      select(periodo,tri,cod_reg,t01,t15,t21,t23,t40,t45,t46,
             t49,t50,t75,t77,t80,t201,t202,t203)
} else if (modelo == 4){
  base_crimes <- base_trimestral %>% 
    filter(ano >(ano_referencia-5)) %>%
    mutate(periodo = paste(ano)) %>% 
    select(periodo,tri,cod_reg,t01,t15,t21,t23,t40,t45,t46,
           t49,t50,t75,t77,t80,t201,t202,t203)
}

base_crimes <- base_crimes %>% 
  group_by(cod_reg, periodo) %>% 
  summarise(hd_vitima = sum(t50),
            hd_ocorr = sum(t21),
            lat_ocorr = sum(t23),
            lat_vitima =sum(t49),
            tot_estupro =sum(t201),
            estupro_vuln =sum(t202),
            roubo_outros =sum(t81),
            roubo_veiculos = sum(t80),
            roubo_total =sum(t77),
            extor_seq = sum(t15),
            lesao_morte = sum(t40)
            ) %>% 
  mutate(regiao = case_when(cod_reg == 10 ~ "Capital", 
                            cod_reg == 20 ~ "Grande São Paulo",
                            cod_reg == 99 ~ "Estado de São Paulo",
                            cod_reg %in% c(20:40) ~ "Interior")) %>% 
  mutate(deinter = case_when(cod_reg == 31 ~ "Deinter 01",
                             cod_reg == 32 ~ "Deinter 02",
                             cod_reg == 33 ~ "Deinter 03",
                             cod_reg == 34 ~ "Deinter 04",
                             cod_reg == 35 ~ "Deinter 05",
                             cod_reg == 36 ~ "Deinter 06",
                             cod_reg == 37 ~ "Deinter 07",
                             cod_reg == 38 ~ "Deinter 08",
                             cod_reg == 39 ~ "Deinter 09",
                             cod_reg == 40 ~ "Deinter 10")) 
base_crimes <- base_crimes %>% 
  mutate (ano= substr(periodo, start = 1, stop = 4))

base_crimes <- base_crimes %>% 
  unite(
    col = "reg_ano",
    cod_reg,ano,
    sep = "-",
    remove = FALSE
  )

###Passo 02: Criando os tabelas de população----

base_pop <- readxl::read_xlsx("../Boletim_sdpa/data-raw/pop_mun.xlsx") |> 
  pivot_longer(cols = starts_with("20"), names_to = "ano") |> 
  rename(Pop = value) |> 
  mutate(
    deinter = case_when(
      departa == "Decap" ~ "Capital",
      departa == "Demacro" ~ "Grande São Paulo", 
      departa == "Deinter 1" ~ "São José dos Campos",
      departa == "Deinter 2" ~ "Campinas",
      departa == "Deinter 3" ~ "Ribeirão Preto"  ,
      departa == "Deinter 4" ~ "Bauru",
      departa == "Deinter 5" ~ "São José do Rio Preto",
      departa == "Deinter 6" ~ "Santos",
      departa == "Deinter 7" ~ "Sorocaba",
      departa == "Deinter 8" ~ "Presidente Prudente",
      departa == "Deinter 9" ~ "Piracicaba",
      departa == "Deinter 10" ~ "Araçatuba")
  )  


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
      deinter == "Araçatuba" ~ 40),
      Pop = as.double(Pop)
  ) |> 
  mutate(regiao = case_when(cod_reg == 10 ~ "Capital", 
                            cod_reg == 20 ~ "Grande São Paulo",
                            cod_reg == 99 ~ "Estado de São Paulo",
                            cod_reg %in% c(20:40) ~ "Interior"))

#Criando as linhas de total do estado e interior

pop_estado <- base_pop %>% 
group_by(ano) %>% 
summarise(pop = sum(Pop))

pop_estado <- pop_estado %>%
  mutate (reg_ano = paste(99,"-", ano, sep = "")) %>% 
  select(reg_ano, pop)

pop_int <- base_pop %>% 
  filter(regiao == "Interior") |> 
  group_by(ano) %>% 
  summarise(pop = sum(Pop))

pop_int <- pop_int %>% 
  mutate (reg_ano = paste(30,"-", ano, sep = "")) |> 
  select(reg_ano, pop)

novas_linhas_pop <- rbind(pop_estado,pop_int)
remove(pop_estado,pop_int)

base_pop <- base_pop %>% 
  unite(
    col = "reg_ano",
    cod_reg,ano,
    sep = "-"
  )

base_pop <- base_pop %>% 
  group_by(reg_ano) %>% 
  summarise(pop = sum(Pop))

base_pop <- rbind(base_pop,novas_linhas_pop)
remove(novas_linhas_pop)

#### Passo 03: Juntando população e crimes ----

base_crimes <- left_join(base_crimes, base_pop, by = "reg_ano")

#### Passo 04: Tratamento dos dados da corregedoria a partir do modelo selecionado ----

base_corregedoria <- read.csv2("../Boletim_sdpa/data-raw/base_corregedoria.csv") %>% 
  mutate(
    let_ser = c1+c3,
    let_fol = c2+c4,
    mort_ser = c14,
    mort_fol = c15,
    cod_reg = case_when(
      departa == "Decap" ~ 10,
      departa == "Demacro" ~ 20,
      departa == "Demacro" ~ 20,
      departa == "Deinter 1" ~ 31,
      departa == "Deinter 2" ~ 32,
      departa == "Deinter 3" ~ 33,
      departa == "Deinter 4" ~ 34,
      departa == "Deinter 5" ~ 35,
      departa == "Deinter 6" ~ 36,
      departa == "Deinter 7" ~ 37,
      departa == "Deinter 8" ~ 38,
      departa == "Deinter 9" ~ 39,
      departa == "Deinter 10" ~ 40),
    trimestre = case_when(
      cod_mes == 1 ~ 1,
      cod_mes == 2 ~ 1,
      cod_mes == 3 ~ 1,
      cod_mes == 4 ~ 2,
      cod_mes == 5 ~ 2,
      cod_mes == 6 ~ 2,
      cod_mes == 7 ~ 3,
      cod_mes == 8 ~ 3,
      cod_mes == 9 ~ 3,
      cod_mes == 10 ~ 4,
      cod_mes == 11 ~ 4,
      cod_mes == 12 ~ 4),
    semestre = case_when(
      cod_mes <7 ~ 1,
      TRUE  ~  2)
  )  

base_corregedoria <- base_corregedoria %>% 
  select(cod_ano,cod_reg,semestre,trimestre,let_ser,let_fol,mort_ser,mort_fol)

base_corregedoria <- base_corregedoria %>% 
  drop_na(cod_reg)

correg_estado <- base_corregedoria %>% 
group_by(cod_ano,trimestre,semestre) %>% 
summarise(let_ser = sum(let_ser, na.rm = TRUE),
          let_fol = sum(let_fol, na.rm = TRUE),
          mort_ser = sum(mort_ser, na.rm = TRUE),
          mort_fol = sum(mort_fol, na.rm = TRUE))
correg_estado <-  correg_estado %>% 
  mutate(cod_reg = 99)

correg_int <- base_corregedoria %>% 
  filter(cod_reg>30) %>% 
  group_by(cod_ano,trimestre,semestre) %>% 
  summarise(let_ser = sum(let_ser, na.rm = TRUE),
            let_fol = sum(let_fol, na.rm = TRUE),
            mort_ser = sum(mort_ser, na.rm = TRUE),
            mort_fol = sum(mort_fol, na.rm = TRUE))
correg_int <-  correg_int %>% 
  mutate(cod_reg = 30)

base_corregedoria <- rbind(base_corregedoria, correg_estado, correg_int)
remove(correg_estado, correg_int)

if (modelo == 1){
  base_corregedoria <- base_corregedoria %>% 
    filter(cod_ano >(ano_referencia-2)) %>%
    mutate(periodo = paste(cod_ano,"/", trimestre, "º Trimestre", sep = ""))
} else if (modelo == 2){
  base_corregedoria <- base_corregedoria %>% 
    filter(cod_ano >(ano_referencia-5)) %>% #alterado para incluir 2018
    mutate(periodo = paste(cod_ano,"/", semestre, "º Semestre", sep = "")) 
} else if (modelo == 3){
  base_corregedoria <- base_corregedoria %>% 
    filter(cod_ano >(ano_referencia-2)) %>%
    mutate(periodo = paste(cod_ano,"/", trimestre, "º Trimestre", sep = ""))
} else if (modelo == 4){
  base_corregedoria <- base_corregedoria %>% 
    filter(cod_ano >(ano_referencia-5)) %>%
    mutate(periodo = paste(cod_ano))
}

base_corregedoria <- base_corregedoria %>% 
  group_by(cod_reg, periodo) %>% 
  summarise(let_ser = sum(let_ser),
            let_fol = sum(let_fol),
            mort_ser = sum(mort_ser),
            mort_fol =sum(mort_fol)
  )

####Passo 05: Juntando base de crimes e da corregedoria----
base_crimes <- base_crimes %>% 
  mutate(id = paste(cod_reg,"-", periodo, sep = ""))

base_corregedoria <- base_corregedoria %>% 
  mutate(id = paste(cod_reg,"-", periodo, sep = ""))

base_completa <- left_join(base_crimes, base_corregedoria,by ="id") |> 
  select(-ends_with(".y"))|> 
  rename(periodo = periodo.x) |> 
  rename(cod_reg = cod_reg.x)

saveRDS(base_completa, "./Análises Especiais/2022_1Sem - Levantamento eleições estaduais/base_eleicao.rds")

# Passo 06: Modelar e exportar bases para excel ---------------------------

base_eleicao_excel <- base_completa %>% 
  select(-reg_ano, -cod_reg, -ano, -pop, -id) %>% 
  write.csv2("./Análises Especiais/2022_1Sem - Levantamento eleições estaduais/base_eleicao.csv")


