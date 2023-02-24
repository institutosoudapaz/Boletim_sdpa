
# Carregar os pacotes -------------------------------------------------------------------------

library(tidyverse)
#library(ggplot2)
#library(stringr)

# Baixar os dados trimestrais da SSP  ---------------------------------------------------------

#Abre a base trimestral

base_trimestral <- read.csv2(
  "../isdp_scraper/output/base_trimestral/base_trimestral_v4_1996-2022.csv")

# Modela a base de acordo com o tipo de relatório (trimestral, semestral ou anual) ------------

ano_referencia <- 2022

base_trimestral <- base_trimestral %>% 
  mutate(
    semestre = case_when(
      tri<3 ~ 1,
      TRUE  ~  2)
    )

# Escolha o modelo adequado:
# 1 para tri01; 2 para Semestre; 3 para tri03 e 4 para Anual

modelo <- 4

# Trata a base trimestral a partir do modelo selecionado
if (modelo == 1){
  base_crimes <- base_trimestral %>% 
    filter(ano >(ano_referencia-2)) %>%
    mutate(periodo = paste(ano,"/", tri, "º Trimestre", sep = "")) %>% 
    select(periodo,tri,cod_reg,t01,t15,t21,t23,t40,t45,t46,
           t49,t50,t75,t77,t80,t201,t202,t203)
} else if (modelo == 2){
  base_crimes <- base_trimestral %>% 
    filter(ano >(ano_referencia-3)) %>%
    mutate(periodo = paste(ano,"/", semestre, "º Semestre", sep = "")) %>% 
    select(periodo,tri,cod_reg,t01,t15,t21,t23,t40,t45,t46,
           t49,t50,t75,t77,t80,t201,t202,t203)
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

# Sumarizar a base por período selecionado (trimestre, semestre ou ano)

base_crimes <- base_crimes %>% 
  group_by(cod_reg, periodo) %>% 
  summarise(hd_vitima = sum(t50),
            hd_ocorr = sum(t21),
            lat_ocorr = sum(t23),
            lat_vitima =sum(t49),
            tot_estupro =sum(t201),
            estupro_vuln =sum(t202),
            roubo_outros =sum(t77),
            roubo_veic = sum(t80),
            extor_seq = sum(t15),
            lesao_morte = sum(t40),
            ap_armas =sum(t01),
            prisoes =sum(t75),
            prisoes_flag =sum(t45),
            prisoes_mandado =sum(t46)
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

# Extrai o ano. Rodar apenas nas análises trimestrais e semestrais
base_crimes <- base_crimes %>% 
  mutate (ano = substr(periodo, start = 1, stop = 4))

base_crimes <- base_crimes%>% 
  unite(
    col = "reg_ano",
    cod_reg,ano,
    sep = "-",
    remove = FALSE
  )


# Passo 02: Criando os tabelas de população -------------------------------

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

# Criar as linhas de total do estado e interior

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

# Juntar as bases de população e de crimes 

base_crimes <- left_join(base_crimes, base_pop, by = "reg_ano")


# Tratar dados da corregedoria a partir do modelo selecionado ---------------------------------

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
  filter(cod_reg > 30) %>% 
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
    filter(cod_ano >(ano_referencia-3)) %>%
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

# Juntar as bases de crimes e da corregedoria

base_crimes <- base_crimes %>% 
  mutate(id = paste(cod_reg,"-", periodo, sep = ""))

base_corregedoria <- base_corregedoria %>% 
  mutate(id = paste(cod_reg,"-", periodo, sep = ""))

base_completa <- left_join(base_crimes, base_corregedoria,by ="id") |> 
  select(-ends_with(".y"))|> 
  rename(periodo = periodo.x) |> 
  rename(cod_reg = cod_reg.x)

saveRDS(base_completa, "./data-raw/base_completa.rds")

# Preparar a base mensal (tem dados dos DPs) --------------------------------------------------

base_mensal <- read.csv2("../isdp_scraper/output/base_mensal/base_mensal_v7_2022.csv")

# Modelagem base mensal 

base_mensal <- base_mensal %>% 
  mutate(
    trimestre = case_when(
      cod_mes ==1 ~ 1,
      cod_mes ==2 ~ 1,
      cod_mes ==3 ~ 1,
      cod_mes ==4 ~ 2,
      cod_mes ==5 ~ 2,
      cod_mes ==6 ~ 2,
      cod_mes ==7 ~ 3,
      cod_mes ==8 ~ 3,
      cod_mes ==9 ~ 3,
      cod_mes ==10 ~ 4,
      cod_mes ==11 ~ 4,
      cod_mes ==12 ~ 4))%>% 
  mutate(
    semestre = case_when(
      trimestre<3 ~ 1,
      TRUE  ~  2)
  ) %>% 
  mutate(
    semestre = case_when(
      trimestre<3 ~ 1,
      TRUE  ~  2),
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
      departa == "Deinter 10" ~ 40)
  )

# Trata a base mensal a partir do modelo selecionado

if (modelo == 1){
  base_mensal <- base_mensal %>% 
    filter(cod_ano >(ano_referencia-2)) %>%
    mutate(periodo = paste(cod_ano,"/", trimestre, "º Trimestre", sep = "")) %>% 
    select(periodo,trimestre,nom_del,nom_mun,pop_mun,cod_reg,o01,o02,o08,o12,o13,o14,o15,
           o16,o18,o19,p05,p09,p10,p11)
} else if (modelo == 2){
  base_mensal <- base_mensal %>% 
    filter(cod_ano >(ano_referencia-3)) %>%
    mutate(periodo = paste(cod_ano,"/", semestre, "º Semestre", sep = "")) %>% 
    select(periodo,trimestre,nom_del,nom_mun,pop_mun,cod_reg,o01,o02,o08,o12,o13,o14,o15,
           o16,o18,o19,p05,p09,p10,p11)
} else if (modelo == 3){
  base_mensal <- base_mensal %>% 
    filter(cod_ano >(ano_referencia-2)) %>%
    mutate(periodo = paste(cod_ano,"/", trimestre, "º Trimestre", sep = "")) %>% 
    select(periodo,trimestre,nom_del,nom_mun,pop_mun,cod_reg,o01,o02,o08,o12,o13,o14,o15,
           o16,o18,o19,p05,p09,p10,p11)
} else if (modelo == 4){
  base_mensal <- base_mensal %>% 
    filter(cod_ano >(ano_referencia-5)) %>%
    mutate(periodo = paste(cod_ano)) %>% 
    select(periodo,trimestre,nom_del,nom_mun,pop_mun,cod_reg,o01,o02,o08,o12,o13,o14,o15,
           o16,o18,o19,p05,p09,p10,p11)
}

# Criar a base mensal por DP, sem população

base_mensal_dp <- base_mensal %>% 
  group_by(periodo, nom_del) %>% 
  summarise(hd_vitima = sum(o02),
            hd_ocorr = sum(o01),
            lat_ocorr = sum(o12),
            lat_vitima =sum(o13),
            tot_estupro =sum(o14),
            estupro_vuln =sum(o16),
            roubo_outros =sum(o18),
            roubo_veic = sum(o19),
            lesao_morte = sum(o08),
            ap_armas =sum(p05),
            prisoes =sum(p11),
            cod_reg = unique(cod_reg))

# Criando o base mensal por municipio, com população.

base_mensal_munic <- base_mensal %>% 
  group_by(periodo, nom_mun) %>% 
  summarise(hd_vitima = sum(o02),
            hd_ocorr = sum(o01),
            lat_ocorr = sum(o12),
            lat_vitima =sum(o13),
            tot_estupro =sum(o14),
            estupro_vuln =sum(o16),
            roubo_outros =sum(o18),
            roubo_veic = sum(o19),
            lesao_morte = sum(o08),
            ap_armas =sum(p05),
            prisoes =sum(p11)) %>% 
  mutate(ano = (str_sub(periodo,start = 1, end = 4)),
         mun_ano = paste(nom_mun,"-", ano, sep = ""))

# CRIAR BASE DE POP MUNICIPIOS NO FORMATO CORRETO
base_pop_mun <- readxl::read_xlsx("./data-raw/pop_mun.xlsx") %>%
  mutate(mun_ano = paste(municipio_nome,"-", Ano, sep = "")) %>% 
  select(mun_ano,Pop)

base_mensal_munic <- left_join(base_mensal_munic, base_pop_mun, by = "mun_ano")

saveRDS(base_mensal_munic, "./data-raw/base_mensal_munic.rds")
saveRDS(base_mensal_dp, "./data-raw/base_mensal_dp.rds")
saveRDS(base_mensal, "./data-raw/base_mensal.rds")

# Dados violência contra a mulher -------------------------------------------------------------

base_viol_mul <- readxl::read_xlsx("./data-raw/vio_mulher.xlsx") %>% 
  select(!Total) %>% 
  pivot_longer(cols = Capital:Interior,
                names_to = "reg",
                values_to = "contador") %>% 
  mutate(cod_reg = case_when(
    reg == "Capital" ~10,
    reg == "Demacro" ~20,
    reg == "Interior" ~30)
  ) %>% 
  select(Sem, Tri, Mês, Ano, cod_reg, item,contador) |> 
  # Selecionar categorias crimes de violencia contra mulher
  filter(item =="HOMICÍDIO DOLOSO - TOTAL" |item ==	"LESÃO CORPORAL DOLOSA")

if (modelo == 1){
  base_viol_mul <- base_viol_mul %>% 
    filter(Ano >(ano_referencia-2)) %>%
    mutate(periodo = paste(Ano,"/", Tri, "º Trimestre", sep = "")) %>% 
    select(periodo,Tri,cod_reg,item, contador)
} else if (modelo == 2){
  base_viol_mul <- base_viol_mul %>% 
    filter(Ano >(ano_referencia-3)) %>%
    mutate(periodo = paste(Ano,"/", Sem, "º Semestre", sep = "")) %>% 
    select(periodo,Tri,cod_reg,item, contador)
} else if (modelo == 3){
  base_viol_mul <- base_viol_mul %>% 
    filter(Ano >(ano_referencia-2)) %>%
    mutate(periodo = paste(Ano,"/", Tri, "º Trimestre", sep = "")) %>% 
    select(periodo,Tri,cod_reg,item, contador)
} else if (modelo == 4){
  base_viol_mul <- base_viol_mul %>% 
    filter(Ano >(ano_referencia-5)) %>%
    mutate(periodo = paste(Ano)) %>% 
    select(periodo,Tri,cod_reg,item, contador)
}

saveRDS(base_viol_mul, "./data-raw/base_viol_mul.rds")

# Modelar e exportar bases para excel ---------------------------------------------------------

base_completa_excel <- readRDS("./data-raw/base_completa.rds") %>% 
  select(-reg_ano, -cod_reg, -ano, -pop, -id) %>% 
  writexl::write_xlsx("./data-raw/base_completa.xlsx")

base_viol_mulher_excel <- readRDS("./data-raw/base_viol_mul.rds") %>% 
  rename(Total = contador) %>% 
  select (-Tri) %>% 
  filter(item %in% c("HOMICÍDIO DOLOSO (exclui FEMINICÍDIO)",
                     "FEMINICÍDIO", 
                     "HOMICÍDIO DOLOSO - TOTAL",
                     "LESÃO CORPORAL DOLOSA")) %>% 
  writexl::write_xlsx("./data-raw/base_viol_mulher.xlsx")

