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
  )

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
    filter(ano >(ano_referencia-6)) %>%
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
            prisoes =sum(t75)
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
base_pop <- base_pop %>% 
  unite(
    col = "reg_ano",
    cod_reg,ano,
    sep = "-"
  )

base_pop <- base_pop %>% 
  group_by(reg_ano) %>% 
  summarise(pop = sum(Pop))

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
    filter(cod_ano >(ano_referencia-6)) %>%
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

####Passo 06: Preparando a base mensal----

base_mensal <- readRDS("../Boletim_sdpa/data-raw/base_mensal.RDS")
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
    select(periodo,trimestre,nom_del,nom_mun,cod_reg,o1,o2,o8,o12,o13,o14,o15,o16,o18,o19,p5,p9,p10,p11)
} else if (modelo == 2){
  base_mensal <- base_mensal %>% 
    filter(cod_ano >(ano_referencia-3)) %>%
    mutate(periodo = paste(cod_ano,"/", semestre, "º Semestre", sep = "")) %>% 
    select(periodo,trimestre,nom_del,nom_mun,cod_reg,o1,o2,o8,o12,o13,o14,o15,o16,o18,o19,p5,p9,p10,p11)
} else if (modelo == 3){
  base_mensal <- base_mensal %>% 
    filter(cod_ano >(ano_referencia-2)) %>%
    mutate(periodo = paste(cod_ano,"/", trimestre, "º Trimestre", sep = "")) %>% 
    select(periodo,trimestre,nom_del,nom_mun,cod_reg,o1,o2,o8,o12,o13,o14,o15,o16,o18,o19,p5,p9,p10,p11)
} else if (modelo == 4){
  base_mensal <- base_mensal %>% 
    filter(cod_ano >(ano_referencia-6)) %>%
    mutate(periodo = paste(cod_ano)) %>% 
    select(periodo,trimestre,nom_del,nom_mun,cod_reg,o1,o2,o8,o12,o13,o14,o15,o16,o18,o19,p5,p9,p10,p11)
}

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
    filter(Ano >(ano_referencia-6)) %>%
    mutate(periodo = paste(Ano)) %>% 
    select(periodo,Tri,cod_reg,item, contador)
}

saveRDS(base_viol_mul, "base_viol_mul")

# CRIAÇÃO DOS GRÁFICOS

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
  coord_flip(ylim=c(100, 6200))

g <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
              textGrob("Letalidade Violenta", x = 0.03, hjust = 0, gp=gpar(fontsize=30, col="white", 
                                                            fontface="bold")))
grid.arrange(g, p, heights=c(1,9))

# Criar gráfico crimes violentos

p <- base_completa %>% 
  filter(periodo.x > (ano_referencia-2)) %>% 
  group_by(periodo.x) %>%
  summarise(tot_estupro = sum(tot_estupro),
            extor_seq = sum(extor_seq),
            hd_ocorr = sum(hd_ocorr),
            lat_ocorr = sum(lat_ocorr),
            roubo_veic = sum(roubo_veic),
            roubo_outros = sum(roubo_outros), 
            total = sum(tot_estupro,extor_seq, hd_ocorr,lat_ocorr, roubo_veic,roubo_outros)) %>% 
  pivot_longer(!periodo.x, names_to = "crime", values_to = "count") %>% 
  ggplot(aes(fill=factor(periodo.x, levels=c("2021", "2020")), y= count, 
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
  coord_flip(ylim=c(100, 330000))

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
  filter(cod_reg.x <31) %>% 
  group_by(regiao, periodo.x) %>%
  summarise(tot_estupro = sum(tot_estupro),
            extor_seq = sum(extor_seq),
            hd_ocorr = sum(hd_ocorr),
            lat_ocorr = sum(lat_ocorr),
            roubo_veic = sum(roubo_veic),
            roubo_outros = sum(roubo_outros)) %>% 
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
  theme_sdpa_macroreg 

g <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
                 textGrob(titulo, x = 0.03, hjust = 0, gp=gpar(fontsize=30, col="white", 
                                          fontface="bold")))

grid.arrange(g, p, heights=c(1,9))

}

grafico_geral(tot_estupro, "Estupros Total")  # Teste da função

# Criar gráfico taxa de crimes por ano/macrorregiao

grafico_taxa_macro<- function(crime, titulo) { #selecionar o tipo de crime e titulo
  # do gráfico
  
p <- base_completa %>% 
  filter(cod_reg.x <31) %>% 
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
  ggplot(aes(fill= periodo.x, y= {{crime}}, x= regiao)) + 
  geom_bar(position="dodge", stat="identity", size=.4, colour="light grey") +
  geom_text(aes(label = round(..y.., 2), vjust = -0.5),  position = position_dodge(0.9))+
  scale_fill_manual(values = cores_2) +
  guides(fill = guide_legend(reverse = TRUE))+
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
        axis.text.y=element_text(size=12),
        legend.title = element_blank(), 
        legend.text=element_text(size=12),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        aspect.ratio=8.7/20,
        plot.margin=unit(c(0.2,0,0,0), 'cm'))

grafico_taxa_deinter <- function(crime, titulo, limite) { #selecionar o tipo de crime e titulo
  # do gráfico

  #PESQUISAR max_y *1,2
  
p <- base_completa %>% 
  filter(cod_reg.x >30) %>% 
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

grafico_taxa_deinter(hd_ocorr, "Homicídios (taxa por 100 mil habitantes)", 14) #teste da função

### GRAFICOS QUE FALTAM

# Criar gráfico top 10 municípios por número absoluto/crime

########Arrumar a base mensal para ficar padronizada com os crimes

# p <- base_mensal %>% 
#   filter(periodo > (ano_referencia-2)) %>% 
#   group_by(nom_mun, periodo) %>%


# Criar gráfico top 10 municípios por taxa/crime

######## Arrumar a base mensal para ficar padronizada com os crimes
######## Arrumar dados da população por município


# Criar gráfico top 10 distritos policiais por número absoluto/crime

######## Arrumar a base mensal para ficar padronizada com os crimes

# p <- base_mensal %>% 
#   filter(periodo > (ano_referencia-2)) %>% 
#   group_by(nom_del, periodo) %>%


# Criar gráfico de letalidade policial em serviço e fora por ano 

p <- base_completa %>% 
  filter(cod_reg.x != 30) %>% 
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


# Criar gráfico da participação da letalidade policial nas mortes cometidas por macroregiões

######## Criar base que sumariza letalidade violenta (homicídio + latrocinio + Lesão corporal seguida de morte)
######## Calcular percentual por macroregião


# Criar módulo de títulos sem os gráficos anexos


titulo <- function(titulo) { #selecionar o titulo

  t <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
                textGrob(titulo, x = 0.03, hjust = 0, gp=gpar(fontsize=30, col="white", 
                                                              fontface="bold")))
  
  grid.arrange(t, heights=c(1,9), clipGrob(1,1))
  
}

titulo("Destaques") 

# Criar mapa de taxas de crime por deinter

theme_sdpa_maps <-  theme_void()+
  theme(#legend.position = "bottom",
        #axis.text.y=element_text(size=12),
        legend.title = element_blank(), 
        legend.text=element_text(size=12),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        #aspect.ratio=8.7/20,
        plot.margin=unit(c(0.2,0,0,0), 'cm'))

# Abrir arquivo shape

shp_deinter <- sf::st_read("./data-raw/shapes/Departamentos_SP.shp", quiet = TRUE)

# teste do shape

shp_deinter %>%
  ggplot() +
  geom_sf(aes())

# Padronizar nomes dos deinter

shp_deinter$DepGeoDes <- as.factor(shp_deinter$DepGeoDes)

levels(shp_deinter$DepGeoDes) <- c("DECAP", "Deinter 01", "Deinter 10", "Deinter 02", "Deinter 03", 
                            "Deinter 04", "Deinter 05", "Deinter 06", "Deinter 07", "Deinter 08",
                            "Deinter 09","DEMACRO")

# Mesclar base_completa e o shape pela coluna de deinter

tab_estado <- base_completa %>% 
  filter(periodo.x > (ano_referencia-1)) %>% 
  filter(cod_reg.x != 30) %>% 
  group_by(deinter) %>%
  right_join(shp_deinter, by = c("deinter" = "DepGeoDes"))

# Mapa
tab_estado %>% 
  #drop_na() %>% 
  summarise(tot_estupro = sum(tot_estupro)/pop*100000,
            extor_seq = sum(extor_seq)/pop*100000,
            hd_ocorr = sum(hd_ocorr)/pop*100000,
            lat_ocorr = sum(lat_ocorr)/pop*100000,
            roubo_veic = sum(roubo_veic)/pop*100000,
            roubo_outros = sum(roubo_outros)/pop*100000,
            geometry = geometry) %>% 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = hd_ocorr))+
  geom_sf_text(aes(geometry = geometry, label = deinter), size = 4, color = "white", nudge_y = 0.1)+
  geom_sf_text(aes(geometry = geometry, label = round(hd_ocorr, 2)), size = 4, color = "white", 
               nudge_y = -0.1) +
  theme_sdpa_maps
 

# Criar mapa de número absoluto de crimes por DP capital

theme_sdpa_maps <-  theme_void()+
  theme(#legend.position = "bottom",
    #axis.text.y=element_text(size=12),
    legend.title = element_blank(), 
    legend.text=element_text(size=12),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    #aspect.ratio=8.7/20,
    plot.margin=unit(c(0.2,0,0,0), 'cm'))

# Abrir arquivo shape

shp_capital <- sf::st_read("./data-raw/shapes/Distrito_policial_SP.shp", quiet = TRUE) %>% 
  filter(DepGeoDes == "DECAP")
  

# teste do shape

shp_capital %>%
  ggplot() +
  geom_sf(aes())




  


