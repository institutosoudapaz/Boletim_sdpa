#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Código para consolidar o Boletim Sou da Paz Analisa     #
# Elaborado por Rafael Rocha e Leonardo de Carvalho       #
# Data de início de desenvolvimento: 13/07/2021           #

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

###Passo 00: Carregar os pacotes ----
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(grid)
library(stringr)
library(tidyr)
library(rvest)
library(sf)

###Passo 01: Baixar os dados trimestrais da SSP e consolidar a base_trimestral: ----
  #Aqui vale pensar em rever o código para torna-lo mais eficiente, não vamos baixar tudo, 
  #Aqui o resultado precisa ser um arquivo chamado "base_trimestral.RDS"

ano_referencia <- 2021

###Passo 02: Abre e trata a base trimestral ----

#Abre a base trimestral e criar a variavel semestre
base_trimestral <- readRDS("../Boletim_sdpa/data-raw/base_trimestral.RDS")
base_trimestral <- base_trimestral %>% 
  mutate(
    semestre = case_when(
      tri<3 ~ 1,
      TRUE  ~  2)
  ) %>% 
  filter(cod_reg !=30)

base_trimestral <- base_trimestral %>% 
  mutate_at(c(3:96), as.numeric)
base_trimestral[is.na(base_trimestral)] <- 0

#Modelo 1 para tri01; 2 para Semestre; 3 para tri03 e 4 para Anual

modelo <- 4

# trata a base trimestral a partir do modelo selecionado
if (modelo == 1){
  base_crimes <- base_trimestral %>% 
    filter(ano >(ano_referencia-2)) %>%
    mutate(periodo = paste(ano,"/", tri, "º Trimestre", sep = "")) %>% 
    select(periodo,tri,cod_reg,t1,t15,t21,t23,t40,t45,t46,
           t49,t50,t75,t77,t80,t201,t202,t203)
} else if (modelo == 2){
  base_crimes <- base_trimestral %>% 
    filter(ano >(ano_referencia-3)) %>%
    mutate(periodo = paste(ano,"/", semestre, "º Semestre", sep = "")) %>% 
    select(periodo,tri,cod_reg,t1,t15,t21,t23,t40,t45,t46,
           t49,t50,t75,t77,t80,t201,t202,t203)
} else if (modelo == 3){
    base_crimes <- base_trimestral %>% 
      filter(ano >(ano_referencia-2)) %>%
      mutate(periodo = paste(ano,"/", tri, "º Trimestre", sep = "")) %>% 
      select(periodo,tri,cod_reg,t1,t15,t21,t23,t40,t45,t46,
             t49,t50,t75,t77,t80,t201,t202,t203)
} else if (modelo == 4){
  base_crimes <- base_trimestral %>% 
    filter(ano >(ano_referencia-5)) %>%
    mutate(periodo = paste(ano)) %>% 
    select(periodo,tri,cod_reg,t1,t15,t21,t23,t40,t45,t46,
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
            roubo_outros =sum(t77),
            roubo_veic = sum(t80),
            extor_seq = sum(t15),
            lesao_morte = sum(t40),
            ap_armas =sum(t1),
            prisoes =sum(t75),
            prisoes_flag =sum(t45),
            prisoes_mandado =sum(t46)
            ) %>% 
  mutate(regiao = case_when(cod_reg == 10 ~ "Capital", 
                            cod_reg == 20 ~ "Grande São Paulo",
                            cod_reg > 20 ~ "Interior")) %>% 
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

base_crimes <- base_crimes%>% 
  unite(
    col = "reg_ano",
    cod_reg,ano,
    sep = "-",
    remove = FALSE
  )

#Criando o total do estado
base_estado <- base_crimes %>% 
  group_by(periodo) %>% 
  filter(cod_reg !=30) %>% 
  summarise(
    hd_vitima = sum(hd_vitima),
    hd_ocorr = sum(hd_ocorr),
    lat_ocorr = sum(lat_ocorr),
    lat_vitima = sum(lat_vitima),
    tot_estupro =sum(tot_estupro),
    estupro_vuln =sum(estupro_vuln),
    roubo_outros =sum(roubo_outros),
    roubo_veic = sum(roubo_veic),
    extor_seq = sum(extor_seq),
    lesao_morte = sum(lesao_morte),
    ap_armas =sum(ap_armas),
    prisoes =sum(prisoes),
    prisoes_flag =sum(prisoes_flag),
    prisoes_mandado =sum(prisoes_mandado)
   ) %>% 
  mutate(cod_reg = 99,
         deinter = "99",
         regiao = "Estado de São Paulo",
         ano = (str_sub(periodo,start = -4)),
         reg_ano = paste(cod_reg,"-", ano, sep = "")
         )

#Criando o total do interior
base_int <- base_crimes %>% 
  filter(cod_reg > 30) %>% 
  group_by(periodo) %>% 
  summarise(
    hd_vitima = sum(hd_vitima),
    hd_ocorr = sum(hd_ocorr),
    lat_ocorr = sum(lat_ocorr),
    lat_vitima = sum(lat_vitima),
    tot_estupro =sum(tot_estupro),
    estupro_vuln =sum(estupro_vuln),
    roubo_outros =sum(roubo_outros),
    roubo_veic = sum(roubo_veic),
    extor_seq = sum(extor_seq),
    lesao_morte = sum(lesao_morte),
    ap_armas =sum(ap_armas),
    prisoes =sum(prisoes),
    prisoes_flag =sum(prisoes_flag),
    prisoes_mandado =sum(prisoes_mandado)
  ) %>% 
  mutate(cod_reg = 30,
         deinter = "30",
         regiao = "Interior",
         ano = (str_sub(periodo,start = -4)),
         reg_ano = paste(cod_reg,"-", ano, sep = "")
  )

  base_crimes <- rbind(base_crimes,base_estado, base_int)
  
  remove(base_estado, base_int)

###Passo 02: Criando os tabelas de população----
base_pop <- readRDS("../Boletim_sdpa/data-raw/pop_munic.RDS")
base_pop <- rename(base_pop, ano = Ano)
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
  )

#Criando as linhas de total do estado e interior
pop_estado <- base_pop
pop_estado <- pop_estado%>% 
group_by(ano) %>% 
summarise(pop = sum(Pop))

pop_estado <- pop_estado%>%
  mutate (reg_ano = paste(99,"-", ano, sep = "")) %>% 
  select(reg_ano, pop)

pop_int <- base_pop
pop_int <- pop_int %>% 
  filter(macrorregiao == "Interior") 
pop_int <- pop_int %>% 
  group_by(ano) %>% 
  summarise(pop = sum(Pop))
pop_int <- pop_int %>% 
mutate (reg_ano = paste(30,"-", ano, sep = "")) 
pop_int <- pop_int %>% 
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

base_corregedoria <- readRDS("../Boletim_sdpa/data-raw/base_corregedoria.RDS") %>% 
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

base_corregedoria <- rbind(base_corregedoria,correg_estado,correg_int)
remove(correg_estado,correg_int)
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

####Passo 05: Juntando base de crimes e da corregedoria----
base_crimes <- base_crimes %>% 
  mutate(id = paste(cod_reg,"-", periodo, sep = ""))

base_corregedoria <- base_corregedoria %>% 
  mutate(id = paste(cod_reg,"-", periodo, sep = ""))

base_completa <- left_join(base_crimes, base_corregedoria,by ="id")

saveRDS(base_completa, "base_completa.rds")

####Passo 06: Preparando a base mensal, para importar a base usar o enconding windows 1512----

base_mensal <- readRDS("../Boletim_sdpa/data-raw/base_mensal.rds")

# modelagem base mensal 

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

# trata a base mensal a partir do modelo selecionado
if (modelo == 1){
  base_mensal <- base_mensal %>% 
    filter(cod_ano >(ano_referencia-2)) %>%
    mutate(periodo = paste(cod_ano,"/", trimestre, "º Trimestre", sep = "")) %>% 
    select(periodo,trimestre,nom_del,nom_mun,pop_mun,cod_reg,o1,o2,o8,o12,o13,o14,o15,o16,o18,o19,p5,p9,p10,p11)
} else if (modelo == 2){
  base_mensal <- base_mensal %>% 
    filter(cod_ano >(ano_referencia-3)) %>%
    mutate(periodo = paste(cod_ano,"/", semestre, "º Semestre", sep = "")) %>% 
    select(periodo,trimestre,nom_del,nom_mun,pop_mun,cod_reg,o1,o2,o8,o12,o13,o14,o15,o16,o18,o19,p5,p9,p10,p11)
} else if (modelo == 3){
  base_mensal <- base_mensal %>% 
    filter(cod_ano >(ano_referencia-2)) %>%
    mutate(periodo = paste(cod_ano,"/", trimestre, "º Trimestre", sep = "")) %>% 
    select(periodo,trimestre,nom_del,nom_mun,pop_mun,cod_reg,o1,o2,o8,o12,o13,o14,o15,o16,o18,o19,p5,p9,p10,p11)
} else if (modelo == 4){
  base_mensal <- base_mensal %>% 
    filter(cod_ano >(ano_referencia-5)) %>%
    mutate(periodo = paste(cod_ano)) %>% 
    select(periodo,trimestre,nom_del,nom_mun,pop_mun,cod_reg,o1,o2,o8,o12,o13,o14,o15,o16,o18,o19,p5,p9,p10,p11)
}

#Criando o base mensal por DP, sem população-
base_mensal_dp <- base_mensal %>% 
  group_by(periodo, nom_del) %>% 
  summarise(hd_vitima = sum(o2),
            hd_ocorr = sum(o1),
            lat_ocorr = sum(o12),
            lat_vitima =sum(o13),
            tot_estupro =sum(o14),
            estupro_vuln =sum(o16),
            roubo_outros =sum(o18),
            roubo_veic = sum(o19),
            lesao_morte = sum(o8),
            ap_armas =sum(p5),
            prisoes =sum(p11),
            cod_reg = unique(cod_reg))

#Criando o base mensal por municipio, com população.

base_mensal_munic <- base_mensal %>% 
  group_by(periodo, nom_mun) %>% 
  summarise(hd_vitima = sum(o2),
            hd_ocorr = sum(o1),
            lat_ocorr = sum(o12),
            lat_vitima =sum(o13),
            tot_estupro =sum(o14),
            estupro_vuln =sum(o16),
            roubo_outros =sum(o18),
            roubo_veic = sum(o19),
            lesao_morte = sum(o8),
            ap_armas =sum(p5),
            prisoes =sum(p11)) %>% 
  mutate(ano = (str_sub(periodo,start = 1, end = 4)),
         mun_ano = paste(nom_mun,"-", ano, sep = ""))

base_pop_mun <- readRDS("../Boletim_sdpa/data-raw/pop_munic.RDS") %>%
  mutate(mun_ano = paste(municipio_nome,"-", Ano, sep = "")) %>% 
  select(mun_ano,Pop)

base_mensal_munic <- left_join(base_mensal_munic, base_pop_mun, by = "mun_ano")

saveRDS(base_mensal_munic, "base_mensal_munic.rds")
saveRDS(base_mensal_dp, "base_mensal_dp.rds")
saveRDS(base_mensal, "base_mensal.rds")

###Passo 07: Dados violência contra a mulher----
base_viol_mul <- readRDS("data-raw/viol_mulher.rds")
base_viol_mul <- base_viol_mul %>% 
  select(!Total)%>% 
  pivot_longer( cols = Capital:Interior,
                names_to = "reg",
                values_to = "contador") %>% 
  mutate(cod_reg = case_when(
    reg == "Capital" ~10,
    reg == "Demacro" ~20,
    reg == "Interior" ~30)
  ) %>% 
  select(Sem, Tri, Mês,Ano,cod_reg,item,contador) %>% 
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

saveRDS(base_viol_mul, "base_viol_mul")

########################### CRIAÇÃO DOS GRÁFICOS ###########################

###Passo 08: Criando os gráficos----

# Definir tema letalidade violenta e cores
theme_sdpa_let <- theme_void()+
  theme(legend.position = "bottom",
        legend.text=element_text(size=12),
        axis.text.y=element_text(size=12),
        legend.title = element_blank(),
        aspect.ratio=11/20)

cores_2 <- c("#be9068", "#042e3f")

# Criar gráfico letalidade violenta

p <- base_completa %>% 
  filter(periodo.x > (ano_referencia-2)) %>% 
  filter(cod_reg.x != 99 & cod_reg.x != 30) %>% 
  group_by(periodo.x) %>%
  summarise(hd_vitima = sum(hd_vitima),
            lat_vitima = sum(lat_vitima),
            lesao_morte = sum(lesao_morte),
            let_ser = sum(let_ser, na.rm = TRUE),
            let_fol = sum (let_fol, na.rm = TRUE),
            total = sum(hd_vitima, lat_vitima, lesao_morte, let_ser, let_fol)) %>% 
  pivot_longer(!periodo.x, names_to = "crime", values_to = "count") %>% 
  ggplot(aes(fill=factor(periodo.x, levels=c("2020", "2021")), y= count, 
             x= factor(crime, levels = c(
               "total", "hd_vitima", "lat_vitima", "lesao_morte", "let_ser", "let_fol"))))+ 
  geom_bar(position="dodge", stat="identity", size=.4, colour="light grey") +
  geom_text(aes(label = format(count, big.mark = ".", scientific = FALSE)), position = position_dodge(0.94), 
            vjust = 0.43, hjust = -0.2,check_overlap = TRUE, size=3.2) +
  scale_fill_manual(values = cores_2) +
  guides(color = "none")+
  scale_x_discrete(labels = str_wrap(
    c("Total de vítimas", "Homicídio doloso", "Latrocínio", "Lesão corporal seguida de morte", 
      "Mortos pela Polícia Civil e Militar em serviço", "Mortos pela Polícia Civil e Militar fora de 
    serviço"), width = 24))+
  theme_sdpa_let +
  # ajustar manualmente, os valores limites são maiores nos relatórios anuais
  coord_flip(ylim=c(100, 4250))

g <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
              textGrob("Letalidade Violenta", x = 0.03, hjust = 0, gp=gpar(fontsize=30, col="white", 
                                                            fontface="bold")))
grid.arrange(g, p, heights=c(1,9))

# Criar gráfico crimes violentos

p <- base_completa %>% 
  filter(periodo.x > (ano_referencia-2)) %>% 
  filter(cod_reg.x != 99 & cod_reg.x != 30) %>% 
  group_by(periodo.x) %>%
  summarise(tot_estupro = sum(tot_estupro),
            extor_seq = sum(extor_seq),
            hd_ocorr = sum(hd_ocorr),
            lat_ocorr = sum(lat_ocorr),
            roubo_veic = sum(roubo_veic),
            roubo_outros = sum(roubo_outros), 
            total = sum(tot_estupro,extor_seq, hd_ocorr,lat_ocorr, roubo_veic,roubo_outros)) %>% 
  pivot_longer(!periodo.x, names_to = "crime", values_to = "count") %>% 
  ggplot(aes(fill=factor(periodo.x, levels=c("2020", "2021")), y= count, 
             x= factor(crime, levels = c(
               "total", "roubo_outros", "roubo_veic","extor_seq", "tot_estupro", "lat_ocorr", "hd_ocorr")))) + 
  geom_bar(position="dodge", stat="identity", size=.4, colour="light grey") +
  geom_text(aes(label = format(count, big.mark = ".", scientific = FALSE)), position = position_dodge(0.94), 
            vjust = 0.43, hjust = -0.2,check_overlap = TRUE, size=3.2) +
  scale_fill_manual(values = cores_2) +
  guides(color = "none")+
  scale_x_discrete(labels = str_wrap(
    c("Total de ocorrências", "Roubo (outros)","Roubo de veículo", "Extorsão mediante sequestro", "Estupro",
      "Latrocínio", "Homicídio doloso"), width = 24))+
  theme_sdpa_let +
  # ajustar manualmente, os valores limites são maiores nos relatórios anuais
  coord_flip(ylim=c(100, 336000))

g <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
              textGrob("Crimes Violentos", x = 0.03, hjust = 0, gp=gpar(fontsize=30, col="white", 
                                                                           fontface="bold")))
grid.arrange(g, p, heights=c(1,9))

# Criar tema SDPA

theme_sdpa_macroreg <- theme_void()+
  theme(legend.position = "bottom",
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(10,10,10,10),
        legend.text=element_text(size=12),
        axis.text.x=element_text(size=12),
        legend.title = element_blank(), 
        aspect.ratio=8.5/20,
        plot.margin=unit(c(0.2,0,0,0), 'cm'))

cores <- c("#cec8c4", "#be9068","#042e3f")

# Criar gráfico crimes por ano/macroregião

grafico_geral <- function(crime, titulo) { #selecionar o tipo de crime e titulo
                                              # do gráfico
p <- base_completa %>% 
  filter(cod_reg.x < 31) %>% 
  group_by(regiao, periodo.x) %>%
  summarise(tot_estupro = sum(tot_estupro),
            extor_seq = sum(extor_seq),
            hd_ocorr = sum(hd_ocorr),
            lat_ocorr = sum(lat_ocorr),
            roubo_veic = sum(roubo_veic),
            roubo_outros = sum(roubo_outros),
            ap_armas = sum(ap_armas),
            prisoes = sum(prisoes)) %>% 
  pivot_longer(cols = regiao, values_to = "regiao") %>% 
  ggplot(aes(fill=factor(regiao, levels=c("Interior", "Grande São Paulo","Capital")),
             y= {{crime}}, x= periodo.x)) + 
  geom_bar(position="stack", stat="identity", size=.4, colour="light grey") +
  geom_text(aes(label = format({{crime}}, big.mark = ".", scientific = FALSE), 
                colour =ifelse(regiao=="Capital", "white", "black")), 
            position=position_stack(vjust=0.5), size=3.4) +
  scale_colour_manual(values=c("white"="white", "black"="black")) +
  # descobrir como colocar o . como divisor aqui
  stat_summary(fun = sum, aes(label = ..y.., group = periodo.x), 
               geom = "text",size=3.4, vjust = -0.5) +
  scale_fill_manual(values = cores) +
  guides(color = "none")+
  theme_sdpa_macroreg +
  guides(fill = guide_legend(reverse = TRUE))

g <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
                 textGrob(titulo, x = 0.03, hjust = 0, gp=gpar(fontsize=30, col="white", 
                                          fontface="bold")))

grid.arrange(g, p, heights=c(1,9))

}

grafico_geral(tot_estupro, "Total de Estupros (ocorrências)")  # Teste da função

# Criar gráfico taxa de crimes por ano/macrorregiao

grafico_taxa_macro<- function(crime, titulo) { #selecionar o tipo de crime e titulo
  # do gráfico
  
p <- base_completa %>% 
  filter(cod_reg.x < 31 | cod_reg.x > 90) %>% 
  filter(periodo.x > (ano_referencia-2)) %>% 
  group_by(regiao, periodo.x) %>%
  # Calcula já a taxa!
    summarise(tot_estupro = sum(tot_estupro)/pop*100000,
              extor_seq = sum(extor_seq)/pop*100000,
              hd_ocorr = sum(hd_ocorr)/pop*100000,
              lat_ocorr = sum(lat_ocorr)/pop*100000,
              roubo_veic = sum(roubo_veic)/pop*100000,
              roubo_outros = sum(roubo_outros)/pop*100000) %>% 
  # Falta criar uma soma do Estado, e os dados de pop do interior
  pivot_longer(cols = regiao, values_to = "regiao") %>% 
  ggplot(aes(fill= periodo.x, y= {{crime}}, x= factor(regiao, levels = c("Capital", "Grande São Paulo",
                                                                         "Interior",
                                                                         "Estado de São Paulo")))) + 
  geom_bar(position="dodge", stat="identity", size=.4, colour="light grey") +
  geom_text(aes(label = round(..y.., 2), vjust = -0.5),  position = position_dodge(0.9))+
  scale_fill_manual(values = cores_2) +
  guides(fill = guide_legend(reverse = FALSE))+
  theme_sdpa_macroreg
  
  g <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
                textGrob(titulo, x = 0.03, hjust = 0, gp=gpar(fontsize=30, col="white", 
                                                              fontface="bold")))
  grid.arrange(g, p, heights=c(1,9))
  
}

grafico_taxa_macro(lat_ocorr, "Taxa de Latrocínio")  # Teste da função

# Criar gráfico taxa de crimes por ano/deinter

theme_sdpa_deinter <- theme_classic()+
  theme(legend.position = "bottom",
        axis.text.y=element_text(size=10.5),
        legend.title = element_blank(), 
        legend.text=element_text(size=12),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        aspect.ratio=11/20,
        plot.margin=unit(c(0.2,0,0,0), 'cm'))

grafico_taxa_deinter <- function(crime, titulo, limite) { #selecionar o tipo de crime e titulo
  # do gráfico

  #PESQUISAR max_y *1,2
  
p <- base_completa %>% 
  filter(cod_reg.x >30 & cod_reg.x != 99) %>% 
  filter(periodo.x > (ano_referencia-2)) %>% 
  group_by(periodo.x, deinter) %>% 
  # Calcula já a taxa!
  summarise(tot_estupro = sum(tot_estupro)/pop*100000,
            extor_seq = sum(extor_seq)/pop*100000,
            hd_ocorr = sum(hd_ocorr)/pop*100000,
            lat_ocorr = sum(lat_ocorr)/pop*100000,
            roubo_veic = sum(roubo_veic)/pop*100000,
            roubo_outros = sum(roubo_outros)/pop*100000,
            prisoes = sum(prisoes)/pop*100000,
            ap_armas = sum(ap_armas)/pop*100000) %>%
  pivot_longer(cols = deinter , values_to = "deinter") %>% 
  ggplot(aes(fill= periodo.x, y= {{crime}}, x= deinter)) + 
  geom_col(width=0.85, position=position_dodge(0.8), size=.4, colour="light grey") +
  geom_text(aes(label = round(..y.., 2)), position = position_dodge(0.94), 
            vjust = 0.43, hjust = -0.5,check_overlap = TRUE, size=3) +
  scale_fill_manual(values = cores_2) +
  guides(color = "none")+
  coord_flip(ylim=c(0, {{limite}})) +
  theme_sdpa_deinter+
  scale_x_discrete(labels = c("Deinter 01 - São José dos Campos", 
      "Deinter 02 - Campinas",
      "Deinter 03 - Ribeirão Preto",
      "Deinter 04 - Bauru",
      "Deinter 05 - São José do Rio Preto",
      "Deinter 06 - Santos",
      "Deinter 07 - Sorocaba",
      "Deinter 08 - Presidente Prudente",
      "Deinter 09 - Piracicaba",
      "Deinter 10 - Araçatuba"))+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, 
                                                    decimal.mark = ','),
                     expand = c(0, 0), n.breaks = 8)

# mudar o separador das taxas de '." para ","

g <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
              textGrob(titulo, x = 0.03, hjust = 0, gp=gpar(fontsize=22, col="white", 
                                                            fontface="bold")))

grid.arrange(g, p, heights=c(1,9))

}

grafico_taxa_deinter(hd_ocorr, "Homicídios (taxa por 100 mil habitantes)", 13.5) #teste da função

# Criar gráfico top 10 municípios por taxa/crime

grafico_10_municipio_taxa <- function(crime, titulo, limite) { #selecionar o tipo de crime e 
                                                                # titulo do gráfico
mun_taxa <- base_mensal_munic %>%
  filter(periodo == ano_referencia) %>% 
  filter(Pop > 99999) %>% 
  group_by(nom_mun) %>% 
  summarise(hd_vitima = sum(hd_vitima)/Pop*100000,
            hd_ocorr = sum(hd_ocorr)/Pop*100000,
            lat_ocorr = sum(lat_ocorr)/Pop*100000,
            lat_vitima =sum(lat_vitima)/Pop*100000,
            tot_estupro =sum(tot_estupro)/Pop*100000,
            estupro_vuln =sum(estupro_vuln)/Pop*100000,
            roubo_outros =sum(roubo_outros)/Pop*100000,
            roubo_veic = sum(roubo_veic)/Pop*100000,
            lesao_morte = sum(lesao_morte)/Pop*100000,
            ap_armas =sum(ap_armas)/Pop*100000,
            prisoes =sum(prisoes)/Pop*100000) %>% 
  arrange(desc({{crime}})) %>% 
  slice_head(n=10) %>% 
  pull(nom_mun)

p <- base_mensal_munic %>%
  filter(periodo > (ano_referencia-2)) %>% 
  filter (nom_mun %in% mun_taxa) %>% 
  group_by(nom_mun, periodo) %>%
  summarise(hd_vitima = sum(hd_vitima)/Pop*100000,
            hd_ocorr = sum(hd_ocorr)/Pop*100000,
            lat_ocorr = sum(lat_ocorr)/Pop*100000,
            lat_vitima =sum(lat_vitima)/Pop*100000,
            tot_estupro =sum(tot_estupro)/Pop*100000,
            estupro_vuln =sum(estupro_vuln)/Pop*100000,
            roubo_outros =sum(roubo_outros)/Pop*100000,
            roubo_veic = sum(roubo_veic)/Pop*100000,
            lesao_morte = sum(lesao_morte)/Pop*100000,
            ap_armas =sum(ap_armas)/Pop*100000,
            prisoes =sum(prisoes)/Pop*100000) %>% 
  ggplot(aes(fill= periodo, y= {{crime}}, x= fct_reorder(nom_mun, {{crime}}, .desc = TRUE)))+
  geom_col(width=0.8, position=position_dodge(0.8), size=.4, colour="light grey") +
  geom_text(aes(label = round(..y.., 2)), position = position_dodge(0.94), 
            vjust = 0.43, hjust = -0.5,check_overlap = TRUE, size=3) +
  scale_fill_manual(values = cores_2) +
  guides(color = "none")+
  coord_flip(ylim=c(0, {{limite}})) +
  theme_sdpa_deinter+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, 
                                                    decimal.mark = ','),
                     expand = c(0, 0), n.breaks = 8)

# mudar o separador das taxas de '." para ","

g <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
              textGrob(titulo, x = 0.03, hjust = 0, gp=gpar(fontsize=15, col="white", 
                                                            fontface="bold")))

grid.arrange(g, p, heights=c(1,9))

}

grafico_10_municipio_taxa(hd_ocorr, "Dez municípios (com mais de 100 mil hab.) com maiores taxas de homicídio doloso – 2021", 25)

# Criar gráfico top 10 distritos policiais por número absoluto/crime

grafico_10_dp <- function(crime, titulo, limite) { #selecionar o tipo de crime e titulo do gráfico
  
  del <- base_mensal %>%
    #na.omit() %>% 
    filter(periodo == ano_referencia) %>% 
    group_by(nom_del) %>% 
    summarise(hd_vitima = sum(o2),
              hd_ocorr = sum(o1),
              lat_ocorr = sum(o12),
              lat_vitima =sum(o13),
              tot_estupro =sum(o14),
              estupro_vuln =sum(o16),
              roubo_outros =sum(o18),
              roubo_veic = sum(o19),
              lesao_morte = sum(o8),
              ap_armas = sum(p5),
              prisoes = sum(p11)) %>% 
    arrange(desc({{crime}})) %>% 
    slice_head(n=10) %>% 
    pull(nom_del)
  
  p <- base_mensal %>%
    #na.omit() %>% 
    filter(periodo > (ano_referencia-2)) %>% 
    group_by(nom_del, periodo) %>%
    filter (nom_del %in% del) %>% 
    summarise(hd_vitima = sum(o2),
              hd_ocorr = sum(o1),
              lat_ocorr = sum(o12),
              lat_vitima =sum(o13),
              tot_estupro =sum(o14),
              estupro_vuln =sum(o16),
              roubo_outros =sum(o18),
              roubo_veic = sum(o19),
              lesao_morte = sum(o8),
              ap_armas =sum(p5),
              prisoes =sum(p11)) %>% 
    ggplot(aes(fill= periodo, y= {{crime}}, x= fct_reorder(nom_del, {{crime}}, .desc = TRUE))) + 
    geom_col(width=0.8, position=position_dodge(0.8), size=.4, colour="light grey") +
    geom_text(aes(label = round(..y.., 2)), position = position_dodge(0.94), 
              vjust = 0.43, hjust = -0.5,check_overlap = TRUE, size=3) +
    scale_fill_manual(values = cores_2) +
    guides(color = "none")+
    coord_flip(ylim=c(0, {{limite}})) +
    theme_sdpa_deinter+
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1, 
                                                      decimal.mark = ','),
                       expand = c(0, 0), n.breaks = 8)
  
  # mudar o separador das taxas de '." para ","
  
  g <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
                textGrob(titulo, x = 0.03, hjust = 0, gp=gpar(fontsize=18, col="white", 
                                                              fontface="bold")))
  
  grid.arrange(g, p, heights=c(1,9))
  
}

grafico_10_dp(hd_ocorr, "Top 10 Homicídios", 43) #teste da função

# Criar gráfico top 10 municípios maior participação de estupros de vulneráveis

top_vuln <- base_mensal_munic %>%
    filter(periodo == ano_referencia) %>% 
    filter(Pop > 99999) %>% 
    group_by(nom_mun) %>% 
    summarise(prop_vuln = estupro_vuln/tot_estupro) %>% 
    arrange(desc(prop_vuln)) %>% 
  slice_head(n=10)%>% 
  pull(nom_mun)

p <- base_mensal_munic %>% 
  filter(periodo > (ano_referencia-2)) %>% 
  group_by(nom_mun, periodo) %>%
  filter (nom_mun %in% top_vuln) %>% 
  summarise(prop_vuln = estupro_vuln/tot_estupro) %>% 
  ggplot(aes(fill= periodo, y= prop_vuln, x= fct_reorder(nom_mun, prop_vuln, .desc = TRUE)))+
  geom_col(width=0.8, position=position_dodge(0.8), size=.4, colour="light grey") +
    geom_text(aes(label = scales::percent(..y.., 0.1)), position = position_dodge(0.94), 
              vjust = 0.43, hjust = -0.5,check_overlap = TRUE, size=3.5) +
  scale_fill_manual(values = cores_2) +
  guides(color = "none")+
  coord_flip(ylim=c(0,1)) +
  theme_sdpa_let+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, 
                                                    decimal.mark = ','),
                     expand = c(0, 0), n.breaks = 8)

  # mudar o separador das taxas de '." para ","
  
  g <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
                textGrob("Dez municípios com mais de 100 mil habitantes com maior participação de vulneráveis entre as vítimas de estupro – 2021" , x = 0.03, hjust = 0, gp=gpar(fontsize=12, col="white", 
                                                              fontface="bold")))
  
  grid.arrange(g, p, heights=c(1,9))
  
# Criar gráfico de proporção de estupros de vulneráveis por região
  
p <- base_completa %>% 
  filter(cod_reg.x < 31 | cod_reg.x > 90) %>% 
  filter(periodo.x == ano_referencia) %>% 
  select(regiao, tot_estupro, estupro_vuln) %>% 
  mutate(prop_vuln = estupro_vuln/tot_estupro) %>% 
  mutate(prop_vulnao = 1-prop_vuln) %>% 
  select(-tot_estupro, - estupro_vuln) %>%  
  gather(type, count, prop_vuln:prop_vulnao) %>% 
  ggplot(aes(x=factor(regiao, levels=c("Estado de São Paulo", "Interior", "Grande São Paulo","Capital")), 
             y=count, fill=forcats::fct_rev(type))) +
  geom_bar(position="stack",  stat="identity", size=.4, colour="light grey") + 
  geom_text(aes(label = scales::percent(..y.., 0.01), 
                colour=ifelse(type=="prop_vuln", "white", "black")), 
            position=position_stack(vjust=0.5), size=4) +
  scale_colour_manual(values=c("white"="white", "black"="black")) +
  scale_fill_manual(labels = c("Estupros de não vulneráveis", "Estupros de vulneráveis"), rev(T), 
                    values = cores_2, ) +
  guides(color = "none")+
  coord_flip() +
  theme_sdpa_let 

g <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
              textGrob("Proporção de estupros de vulneráveis por região – 2021", x = 0.03, hjust = 0, gp=gpar(fontsize=18, col="white", 
                                                            fontface="bold")))

grid.arrange(g, p, heights=c(1,9))

# Criar gráficos de violência contra mulheres por macroregião

vio_mulher <- function(crime, titulo) { #selecionar o tipo de crime e titulo
  # do gráfico

p <- base_viol_mul %>% 
  mutate(across(where(is.character), str_remove_all, pattern = fixed(" "))) %>% 
  mutate(regiao = case_when(cod_reg == 10 ~ "Capital", 
                            cod_reg == 20 ~ "Grande São Paulo",
                            cod_reg > 20 ~ "Interior")) %>% 
  group_by(regiao, periodo, item) %>%
  summarise(total = sum(contador)) %>% 
  filter(item == {{crime}}) %>% 
  ggplot(aes(fill=factor(regiao, levels=c("Interior", "Grande São Paulo","Capital")),
               y= total, x= periodo)) + 
    geom_bar(position="stack", stat="identity", size=.4, colour="light grey") +
    geom_text(aes(label = format(total, big.mark = ".", scientific = FALSE), 
                  colour =ifelse(regiao=="Capital", "white", "black")), 
              position=position_stack(vjust=0.5), size=3.4) +
    scale_colour_manual(values=c("white"="white", "black"="black")) +
    # descobrir como colocar o . como divisor aqui
    stat_summary(fun = sum, aes(label = ..y.., group = periodo), 
                 geom = "text",size=3.4, vjust = -0.5) +
    scale_fill_manual(values = cores) +
    guides(color = "none")+
    theme_sdpa_macroreg +
    guides(fill = guide_legend(reverse = TRUE))
  
  g <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
                textGrob(titulo, x = 0.03, hjust = 0, gp=gpar(fontsize=22, col="white", 
                                                              fontface="bold")))
  
  grid.arrange(g, p, heights=c(1,9))
  
}

# as opções são "LESÃOCORPORALDOLOSA" e "HOMICÍDIODOLOSO-TOTAL" (com aspas mesmo)
vio_mulher("HOMICÍDIODOLOSO-TOTAL", "Lesão corporal dolosa") 

# Criar gráfico da participação da letalidade policial nas mortes cometidas por macroregiões

p <- base_completa %>% 
  filter(periodo.x == ano_referencia) %>% 
  filter(cod_reg.x < 31 | cod_reg.x > 90) %>% 
  group_by(cod_reg.x) %>% 
  mutate(letalidade_tot = sum(hd_vitima, lat_vitima, lesao_morte, let_ser, let_fol)) %>% 
  mutate(letalidade_crime = sum(hd_vitima, lat_vitima, lesao_morte)) %>%
  mutate(letalidade_pol = sum(let_ser, let_fol)) %>% 
  select(cod_reg.x, regiao, letalidade_tot, letalidade_pol,letalidade_crime) %>% 
  mutate(prop_crime = letalidade_crime/letalidade_tot) %>% 
  mutate(prop_apol = letalidade_pol/letalidade_tot) %>% 
  select(-letalidade_tot, -letalidade_pol, -letalidade_crime) %>%  
  gather(type, count, prop_crime:prop_apol) %>% 
  ggplot(aes(x=factor(regiao, levels=c("Estado de São Paulo", "Interior", "Grande São Paulo","Capital")), 
             y=count, fill=forcats::fct_rev(type))) +
  geom_bar(position="stack",  stat="identity", size=.4, colour="light grey") + 
  geom_text(aes(label = scales::percent(..y.., 0.01), 
                colour=ifelse(type=="prop_apol", "white", "black")), 
            position=position_stack(vjust=0.5), size=3.8) +
  scale_colour_manual(values=c("white"="white", "black"="black")) +
  scale_fill_manual(labels = c("Letalidade Violenta (Homicídios Dolosos, Latrocínios e \nLesões Corporais Seguidas de Morte)", "Letalidade Policial"), rev(T), 
                    values = cores_2, ) +
  guides(color = "none")+
  coord_flip() +
  theme_sdpa_let 

g <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
              textGrob("Participação das mortes por policiais na letalidade violenta – 2021", x = 0.03, hjust = 0, gp=gpar(fontsize=18, col="white", 
                                                                                                              fontface="bold")))

grid.arrange(g, p, heights=c(1,9))

# Criar gráfico da porcentagem de pessoas mortas por policiais em serviço e fora de serviço por região

p <- base_completa %>% 
  filter(periodo.x == ano_referencia) %>% 
  filter(cod_reg.x < 31 | cod_reg.x > 90) %>% 
  group_by(cod_reg.x) %>% 
  mutate(letal_tot = sum(let_ser, let_fol)) %>% 
  mutate(prop_aser = let_ser/letal_tot) %>% 
  mutate(prop_fora = let_fol/letal_tot) %>% 
  select(cod_reg.x, regiao, letal_tot, prop_aser,prop_fora) %>% 
  gather(type, count, prop_aser:prop_fora) %>% 
  ggplot(aes(x=factor(regiao, levels=c("Estado de São Paulo", "Interior", "Grande São Paulo","Capital")), 
             y=count, fill=forcats::fct_rev(type))) +
  geom_bar(position="stack",  stat="identity", size=.4, colour="light grey") + 
  geom_text(aes(label = scales::percent(..y.., 0.01), 
                colour=ifelse(type=="prop_aser", "white", "black")), 
            position=position_stack(vjust=0.5), size=3.8) +
  scale_colour_manual(values=c("white"="white", "black"="black")) +
  scale_fill_manual(labels = c("Mortes cometidas por policiais \nfora de serviço", "Mortes cometidas por policiais \nem serviço"), 
                    values = cores_2, ) +
  guides(color = "none")+
  coord_flip() +
  theme_sdpa_let 

g <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
              textGrob("Porcentagem de pessoas mortas por policiais em serviço e fora de serviço – 2021", 
                       x = 0.03, hjust = 0, gp=gpar(fontsize=15, col="white", fontface="bold")))

grid.arrange(g, p, heights=c(1,9))

# Criar gráfico de letalidade policial em serviço e fora por ano 

p <- base_completa %>% 
  filter(cod_reg.x != 30 & cod_reg.x != 99) %>% 
  group_by(periodo.x) %>%
  summarise(let_ser = sum(let_ser),
            let_fol = sum(let_fol)) %>% 
  pivot_longer(cols = let_ser:let_fol, values_to = "count", names_to = "letalidade") %>% 
  ggplot(aes(fill=letalidade, x=periodo.x, y= count))+
  geom_bar(position="stack", stat="identity", size=.4, colour="light grey") +
  geom_text(aes(label = count, colour =ifelse(letalidade == "let_fol", "black", "white")), 
            position=position_stack(vjust=0.5)) +
  scale_colour_manual(values=c("white"="white", "black"="black")) +
  stat_summary(fun = sum, aes(label = ..y.., group = periodo.x), 
               geom = "text",size=4, vjust = -0.5) +
  guides(color = "none")+
  scale_fill_manual(labels = c("Letalidade fora de serviço", "Letalidade em serviço"), 
                    values = cores_2) +
  guides(fill = guide_legend(reverse = TRUE))+
  theme_sdpa_macroreg

g <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
              textGrob("Letalidade Policial", x = 0.03, hjust = 0, 
                       gp=gpar(fontsize=22, col="white", fontface="bold")))

grid.arrange(g, p, heights=c(1,9))

# Criar gráfico da proporção de tipo de prisão por região

p <- base_completa %>% 
  filter(periodo.x == ano_referencia) %>% 
  filter(cod_reg.x < 31 | cod_reg.x > 90) %>% 
  group_by(cod_reg.x) %>% 
  mutate(prisao_tot = sum(prisoes_flag, prisoes_mandado)) %>% 
  mutate(prop_mand = prisoes_mandado/prisao_tot) %>% 
  mutate(prop_flag = prisoes_flag/prisao_tot) %>% 
  select(cod_reg.x, regiao, prisao_tot, prop_flag,prop_mand) %>% 
  gather(type, count, prop_flag:prop_mand) %>% 
  ggplot(aes(x=factor(regiao, levels=c("Estado de São Paulo", "Interior", "Grande São Paulo","Capital")), 
             y=count, fill=forcats::fct_rev(type))) +
  geom_bar(position="stack",  stat="identity", size=.4, colour="light grey") + 
  geom_text(aes(label = scales::percent(..y.., 0.01), 
                colour=ifelse(type=="prop_flag", "white", "black")), 
            position=position_stack(vjust=0.5), size=3.8) +
  scale_colour_manual(values=c("white"="white", "black"="black")) +
  scale_fill_manual(labels = c("Prisões por Mandado", "Prisões por Flagrante"), 
                    values = cores_2, ) +
  guides(color = "none")+
  coord_flip() +
  theme_sdpa_let 

g <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
              textGrob("Proporção de tipo de prisão por região – 2021", 
                       x = 0.03, hjust = 0, gp=gpar(fontsize=22, col="white", fontface="bold")))

grid.arrange(g, p, heights=c(1,9))


# Criar módulo de títulos sem os gráficos anexos


titulo <- function(titulo) { #selecionar o titulo

  t <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
                textGrob(titulo, x = 0.03, hjust = 0, gp=gpar(fontsize=30, col="white", 
                                                              fontface="bold")))
  
  grid.arrange(t, heights=c(1,9), clipGrob(1,1))
  
}

titulo("Destaques") 


# MAPAS ---------------------------------------------------------------------------------------

# Criar mapa de taxas de crime por deinter

theme_sdpa_maps <-  theme_void()+
  theme(legend.text=element_text(size=12),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.margin=unit(c(0.2,0,0,0), 'cm'))

# Abrir arquivo shape

shp_deinter <- sf::st_read("./data-raw/shapes/Departamentos_SP.shp", quiet = TRUE)

# teste do shape

shp_deinter %>%
  ggplot() +
  geom_sf(aes())

# Padronizar nomes dos deinter

shp_deinter$DepGeoDes <- as.factor(shp_deinter$DepGeoDes)

shp_deinter <- shp_deinter %>% 
  filter(DpGeoCod != 30213)

levels(shp_deinter$DepGeoDes) <- c("", "Deinter 01", "Deinter 10", "Deinter 02", "Deinter 03", 
                            "Deinter 04", "Deinter 05", "Deinter 06", "Deinter 07", "Deinter 08",
                            "Deinter 09","")

# Mesclar base_completa e o shape pela coluna de deinter

tab_estado <- base_completa %>% 
  filter(periodo.x > (ano_referencia-1)) %>% 
  filter(cod_reg.x != 30 & cod_reg.x != 99 ) %>%   
  group_by(deinter) %>%
  right_join(shp_deinter, by = c("deinter" = "DepGeoDes"))

# Mapa

mapa_deinter <- function(crime, titulo) { #selecionar o tipo de crime e titulo do gráfico

p <- tab_estado %>% 
  summarise(tot_estupro = sum(tot_estupro)/pop*100000, 
            extor_seq = sum(extor_seq)/pop*100000,
            hd_ocorr = sum(hd_ocorr)/pop*100000,
            lat_ocorr = sum(lat_ocorr)/pop*100000,
            roubo_veic = sum(roubo_veic)/pop*100000,
            roubo_outros = sum(roubo_outros)/pop*100000, 
            geometry = geometry) %>%
 ggplot() +
  geom_sf(aes(geometry = geometry, fill = {{crime}}), colour = "black")+
  geom_sf_text(aes(geometry = geometry, label = deinter), size = 3.5, color = "white", nudge_y = -0.05, nudge_x = 0.05)+
  geom_sf_text(aes(geometry = geometry, label = round({{crime}}, 2)), size = 3, color = "white", 
               nudge_y = -0.25, nudge_x = 0.05)+
  scale_fill_continuous((scales::breaks_extended()), labels = scales::label_comma(), 
                        name = "Taxa por \n100 mil hab.", 
                        guide = guide_colourbar(barheight = unit(4.5, "cm"), barwidth = unit(0.6, "cm")),
                        low = "#8DB0C5", high = "#042e3f")+
  theme_sdpa_maps


g <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
                textGrob(titulo, x = 0.03, hjust = 0, gp=gpar(fontsize=22, col="white", 
                                                              fontface="bold")))
  
  grid.arrange(g, p, heights=c(1,9))
  
}

mapa_deinter(hd_ocorr, "Taxa de homicídios")

# Criar mapa de número absoluto de crimes por DP capital

# Abrir arquivo shape

shp_capital <- sf::st_read("./data-raw/shapes/Distrito_policial_SP.shp", quiet = TRUE) %>% 
  filter(DepGeoDes == "DECAP")

shp_capital %>%
  ggplot() +
  geom_sf(aes())
  
# Padronizar nomes dos DPs

shp_capital$DpGeoDes <- as.factor(shp_capital$DepGeoDes)

shp_capital$DpGeoDes <- c("010 DP - Penha de França",              
                                  "081 DP - Belém",         
                                  "052 DP - Parque São Jorge",       
                                  "090 DP - Parque Novo Mundo",                            
                                  "039 DP - Vila Gustavo",                                 
                                  "019 DP - Vila Maria",                                   
                                  "013 DP - Casa Verde",                                   
                                  "009 DP - Carandiru",                                    
                                  "093 DP - Jaguaré",                                      
                                  "091 DP - Ceagesp",                                      
                                  "089 DP - Portal do Morumbi",                            
                                  "087 DP - Vila Pereira Barreto",                         
                                  "007 DP - Lapa",                                         
                                  "046 DP - Perus",                                        
                                  "034 DP - Vila Sonia",                                   
                                  "033 DP - Pirituba",                                     
                                  "023 DP - Perdizes",                                     
                                  "016 DP - Vila Clementino",                              
                                  "097 DP - Americanópolis",                           
                                  "096 DP - Monções",                                      
                                  "095 DP - Heliópolis",                                   
                                  "083 DP - Parque Bristol",
                                  "036 DP - Vila Mariana",                                 
                                  "035 DP - Jabaquara",                                    
                                  "027 DP - Campo Belo",                                   
                                  "026 DP - Sacomã",                                       
                                  "017 DP - Ipiranga",                                     
                                  "008 DP - Brás",                                         
                                  "077 DP - Santa Cecília",                               
                                  "006 DP - Cambuci",                                      
                                  "003 DP - Campos Elísios",                               
                                  "062 DP - Ermelino Matarazzo",                           
                                  "059 DP - Jardim Noemia",                                
                                  "050 DP - Itaim Paulista",                               
                                  "098 DP - Jardim Míriam",                                
                                  "080 DP - Vila Joaniza",                                 
                                  "025 DP - Parelheiros",                                  
                                  "101 DP - Jardim das Imbuias",                           
                                  "102 DP - Socorro",                                      
                                  "043 DP - Cidade Ademar",                                
                                  "048 DP - Cidade Dutra",                                 
                                  "011 DP - Santo Amaro",                                  
                                  "099 DP - Campo Grande",                                 
                                  "015 DP - Itaim Bibi",                                   
                                  "051 DP - Butantã",                                      
                                  "045 DP - Vila Brasilândia",                             
                                  "038 DP - Vila Amália",                                  
                                  "074 DP - Jaraguá",                                      
                                  "040 DP - Vila Santa Maria",                             
                                  "028 DP - Freguesia do Ó",                               
                                  "072 DP - Vila Penteado",                                
                                  "100 DP - Jardim Herculano",                             
                                  "085 DP - Jardim Mirna",                                
                                  "002 DP - Bom Retiro",                                   
                                  "012 DP - Pari",                                         
                                  "073 DP - Jaçanã",                                       
                                  "020 DP - Água Fria",                                    
                                  "075 DP - Jardim Arpoador",                              
                                  "047 DP - Capão Redondo",                                
                                  "092 DP - Parque Santo Antônio",                         
                                  "037 DP - Campo Limpo",                                  
                                  "018 DP - Alto da Moóca",                                
                                  "044 DP - Guaianazes",                                   
                                  "067 DP - Jardim Robru",                                 
                                  "068 DP - Lajeado",                                      
                                  "053 DP - Parque do Carmo",                              
                                  "069 DP - Teotônio Vilela",                              
                                  "070 DP - Vila Ema",                                     
                                  "055 DP - Parque São Rafael",                            
                                  "054 DP - Cidade Tiradentes",                            
                                  "049 DP - São Mateus",                                   
                                  "103 DP - Cohab Itaquera",                               
                                  "065 DP - Artur Alvim",                                  
                                  "064 DP - Cidade A E Carvalho",                          
                                  "066 DP - Vale do Aricanduva",                           
                                  "041 DP - Vila Rica",                                    
                                  "021 DP - Vila Matilde",                                 
                                  "042 DP - Parque São Lucas",                             
                                  "031 DP - Vila Carrão",                                  
                                  "029 DP - Vila Diva",                                    
                                  "030 DP - Tatuapé",                                      
                                  "058 DP - Vila Formosa",                                 
                                  "057 DP - Parque da Moóca",                              
                                  "056 DP - Vila Alpina",                                  
                                  "032 DP - Itaquera",                                     
                                  "063 DP - Vila Jacuí",                                   
                                  "022 DP - São Miguel Paulista",                          
                                  "024 DP - Ponte Rasa",                                   
                                  "005 DP - Aclimação",                                    
                                  "004 DP - Consolação", 
                                  "078 DP - Jardins",                                      
                                  "014 DP - Pinheiros",                                    
                                  "001 DP - Sé")

# Mesclar base_completa e o shape pela coluna de nome dos DPs

tab_dp <- base_mensal_dp %>% 
  filter(periodo == ano_referencia) %>% 
  filter(cod_reg == 10) %>% 
  right_join(shp_capital, by = c("nom_del" = "DpGeoDes")) %>% 
  mutate(legenda = stringr::str_extract(nom_del,"^.{3}"))

# Mapa

mapa_dp <- function(crime, titulo) { #selecionar o tipo de crime e titulo do gráfico
  
  p <- tab_dp %>% 
    ggplot() +
    geom_sf(aes(geometry = geometry, fill = {{crime}}))+
    geom_sf_text(aes(geometry = geometry, label =  legenda), size = 1.5, color = "white")+
    scale_fill_continuous((scales::breaks_extended()), labels = scales::label_comma(), 
                          name = "Número de \nOcorrências", 
                          guide = guide_colourbar(barheight = unit(4.5, "cm"), barwidth = unit(0.6, "cm")),
                          low = "#8DB0C5", high = "#042e3f")+
    theme_sdpa_maps
  
  g <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
                textGrob(titulo, x = 0.03, hjust = 0, gp=gpar(fontsize=16, col="white", 
                                                              fontface="bold")))
  
  grid.arrange(g, p, heights=c(1,9))
  
}

mapa_dp(hd_ocorr, "Distribuição das ocorrências de homicídios dolosos na capital - 2021")


# Gráficos que não utilizamos -----------------------------------------------------------------

# Criar gráfico top 10 municípios por número absoluto/crime

grafico_10_municipio <- function(crime, titulo, limite) { #selecionar o tipo de crime e titulo do gráfico
  
  mun <- base_mensal %>%
    #na.omit() %>% 
    filter(periodo == ano_referencia) %>% 
    filter(nom_mun != "São Paulo") %>% 
    group_by(nom_mun) %>% 
    summarise(hd_vitima = sum(o2),
              hd_ocorr = sum(o1),
              lat_ocorr = sum(o12),
              lat_vitima =sum(o13),
              tot_estupro =sum(o14),
              estupro_vuln =sum(o16),
              roubo_outros =sum(o18),
              roubo_veic = sum(o19),
              lesao_morte = sum(o8),
              ap_armas =sum(p5),
              prisoes =sum(p11)) %>% 
    arrange(desc({{crime}})) %>% 
    slice_head(n=10) %>% 
    pull(nom_mun)
  
  p <- base_mensal %>%
    #na.omit() %>% 
    filter(periodo > (ano_referencia-2)) %>% 
    group_by(nom_mun, periodo) %>%
    filter (nom_mun %in% mun) %>% 
    summarise(hd_vitima = sum(o2),
              hd_ocorr = sum(o1),
              lat_ocorr = sum(o12),
              lat_vitima =sum(o13),
              tot_estupro =sum(o14),
              estupro_vuln =sum(o16),
              roubo_outros =sum(o18),
              roubo_veic = sum(o19),
              lesao_morte = sum(o8),
              ap_armas =sum(p5),
              prisoes =sum(p11)) %>% 
    ggplot(aes(fill= periodo, y= {{crime}}, x= fct_reorder(nom_mun, {{crime}}, .desc = TRUE))) + 
    geom_col(width=0.8, position=position_dodge(0.8), size=.4, colour="light grey") +
    geom_text(aes(label = round(..y.., 2)), position = position_dodge(0.94), 
              vjust = 0.43, hjust = -0.5,check_overlap = TRUE, size=3) +
    scale_fill_manual(values = cores_2) +
    guides(color = "none")+
    coord_flip(ylim=c(0, {{limite}})) +
    theme_sdpa_deinter+
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1, 
                                                      decimal.mark = ','),
                       expand = c(0, 0), n.breaks = 8)
  
  # mudar o separador das taxas de '." para ","
  
  g <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
                textGrob(titulo, x = 0.03, hjust = 0, gp=gpar(fontsize=22, col="white", 
                                                              fontface="bold")))
  
  grid.arrange(g, p, heights=c(1,9))
  
}

grafico_10_municipio(hd_ocorr, "Top 10 Homicídios", 135) #teste da função

# Tabela de vitimização e letalidade policial -------------------------------------------------

base_tab <- base_completa %>%
  filter(periodo.x > ano_referencia-2) %>% 
  filter(cod_reg.x == 10 | cod_reg.x == 99) %>% 
  select(periodo.x, cod_reg.x, let_ser,let_fol,mort_ser,mort_fol) %>% 
  
  
  pivot_longer(
    cols = let_ser:mort_fol,
    names_to = "crime",
    values_to = "count") %>% 
  
  base_tab %>% 
  knitr::kable()

base_corregedoria_tabela <- base_corregedoria_tabela %>% 
  
  
  base_corregedoria_tabela%>% 
  knitr::kable()
