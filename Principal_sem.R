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

###Passo 01: Baixar os dados trimestrais da SSP e consolidar a base_trimestral: ----
  #Aqui vale pensar em rever o código para torna-lo mais eficiente, não vamos baixar tudo, 
  #só o período mais recente e empilhar com  a base que ja temos

###Passo 02: selecionar o ano e os demais marcos de tempo ----

base_trimestral <- readRDS("../Boletim_sdpa/data-raw/base_trimestral.RDS")
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

base_crimes <- base_crimes %>% 
  group_by(cod_reg,ano, semestre) %>% 
  summarise(hd_ocorr = sum(t21),
            hd_vitima = sum(t50),
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
  mutate(regiao = case_when(cod_reg == 10 ~ "Capital", #cria coluna macroregião
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
                             cod_reg == 40 ~ "Deinter 10")) %>% 
  unite(ano_semestre, c(ano, semestre), sep = " / ", remove = FALSE) 

base_crimes$ano_semestre <- paste(base_crimes$ano_semestre, 'º Semestre', sep='')

base_crimes <- base_crimes %>% 
  unite(
    col = "reg_ano",
    cod_reg,ano,
    sep = "-",
    remove = FALSE
  )

###Passo 04: Criando os tabelas de população----
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
filter(ano >(ano_referencia-3)) %>% 
  unite(
    col = "reg_ano",
    cod_reg,ano,
    sep = "-"
  )

base_pop <- base_pop %>% 
  group_by(reg_ano) %>% 
  summarise(pop = sum(Pop))


#### Passo 05: Juntando população e crimes ----

base_crimes <- left_join(base_crimes, base_pop, by = "reg_ano")


#### Passo 06: Criando a tabela de letalidade e vit policial ----
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
  filter(cod_ano >(ano_referencia-3)) %>% 
  group_by(cod_reg,semestre, cod_ano) %>% 
  summarise(let_ser = sum(let_ser),
            let_fol = sum(let_fol),
            mort_ser = sum(mort_ser),
            mort_fol = sum(mort_fol)) %>% 
  unite(ano_semestre, c(cod_ano, semestre), sep = " / ", remove = FALSE) 

base_corregedoria$ano_semestre <- paste(base_corregedoria$ano_semestre, 'º Semestre', sep='')

saveRDS(base_crimes, "base_crimes.rds") 
saveRDS(base_corregedoria, "base_corregedoria.rds") 

###Passo 07: Criando base de crimes e letalidade violenta----

base_letalidade <- left_join(base_crimes, base_corregedoria, by=c("ano_semestre", "cod_reg")) %>% 
  select(reg_ano, ano, cod_reg, ano_semestre, let_ser, let_fol, lesao_morte, hd_vitima, lat_vitima) %>% 
  filter(ano >(ano_referencia-2)) %>% 
  filter(cod_reg!="30") %>%
  group_by(ano_semestre) %>% 
  summarise(let_ser = sum(let_ser),
            let_fol = sum(let_fol),
            lesao_morte = sum(lesao_morte),
            hd_vitima = sum(hd_vitima),
            lat_vitima = sum(lat_vitima))

base_letalidade$Total <- rowSums(base_letalidade[2:6])

base_letalidade_long <- base_letalidade %>% 
  pivot_longer(!ano_semestre, names_to = "morte", values_to = "count")

base_crimes_long <- base_crimes %>% 
  select(reg_ano, ano, cod_reg, ano_semestre, roubo_outros, roubo_veic, lat_ocorr, hd_ocorr, 
         extor_seq, tot_estupro) %>% 
  filter(ano >(ano_referencia-2)) %>% 
  filter(cod_reg!="30") %>%
  group_by(ano_semestre) %>% 
  summarise(roubo_outros = sum(roubo_outros),
            roubo_veic = sum(roubo_veic),
            lat_ocorr = sum(lat_ocorr),
            hd_ocorr = sum(hd_ocorr),
            extor_seq = sum(extor_seq),
            tot_estupro = sum(tot_estupro))

base_crimes_long$Total <- rowSums(base_crimes_long[2:7])

base_crimes_long <- base_crimes_long %>% 
  pivot_longer(!ano_semestre, names_to = "crime", values_to = "count")

base_corregedoria_long <- base_corregedoria %>% 
  filter(cod_ano >(ano_referencia-2)) %>% 
  select(cod_reg, ano_semestre, let_ser, let_fol, mort_ser, mort_fol) %>% 
  filter(cod_reg!="30") %>%
  group_by(ano_semestre) %>% 
  summarise(let_ser = sum(let_ser),
            let_fol = sum(let_fol),
            mort_ser = sum(mort_ser),
            mort_fol = sum(mort_fol))

base_corregedoria_long <- base_corregedoria_long%>% 
  pivot_longer(!ano_semestre, names_to = "let_vit", values_to = "count")


saveRDS(base_letalidade_long, "base_letalidade_long.rds") 
saveRDS(base_crimes_long, "base_crimes_long.rds") 
saveRDS(base_corregedoria_long, "base_corregedoria_long.rds") 

###Passo 08: Dados violência contra a mulher----
viol_mulher <- readRDS("../Boletim_sdpa/data-raw/viol_mulher.RDS")

base_viol_mulher <- viol_mulher %>% 
  filter(Ano >(ano_referencia-3)) 

base_viol_mulher <- subset(base_viol_mulher, select = -Total)
base_viol_mulher <- base_viol_mulher %>% 
  pivot_longer(
    cols = Capital:Interior,
    names_to = "area",
    values_to = "qdte"
  ) 
  
base_viol_mulher <- base_viol_mulher %>% 
  unite(ano_semestre, c(Ano, Sem), sep = " / ", remove = FALSE) %>% 
  select(ano_semestre, item, area, qdte)

base_viol_mulher$ano_semestre <- paste(base_viol_mulher$ano_semestre, 'º Semestre', sep='')

base_viol_mulher_long <- base_viol_mulher %>% 
  filter(item =="HOMICÍDIO DOLOSO - TOTAL"| item =="LESÃO CORPORAL DOLOSA") %>% 
  group_by(ano_semestre, item, area) %>% 
  summarise(qtde = sum(qtde)) 


base_viol_mulher_long <- base_viol_mulher_long %>%
  pivot_wider(
    names_from = item,
    values_from = qtde)

base_viol_mulher_long$area [base_viol_mulher_long$area =="Demacro"] <- "Grande São Paulo"

saveRDS(base_viol_mulher_long, "base_viol_mulher_long.rds") 

###Passo 09: Criando os gráficos----

# Definir tema letalidade violenta e cores

theme_sdpa_let <- theme_void()+
  theme(legend.position = "bottom",
        legend.text=element_text(size=12),
        axis.text.y=element_text(size=12),
        legend.title = element_blank(),
        aspect.ratio=11/20)


cores_2 <- c("#042e3f", "#be9068")

# Criar gráfico letalidade violenta

p <- base_letalidade_long %>% 
  filter(ano_semestre!="2020 / 2º Semestre") %>% 
  ggplot(aes(fill=factor(ano_semestre,  levels=c("2021 / 1º Semestre", "2020 / 1º Semestre")), y= count, 
             x= factor(morte, levels = c(
               "Total", "hd_vitima", "lat_vitima", "lesao_morte", "let_ser", "let_fol")))) + 
  geom_bar(position="dodge", stat="identity", size=.4, colour="light grey") +
  geom_text(aes(label = count), position = position_dodge(0.94), 
            vjust = 0.43, hjust = -0.5,check_overlap = TRUE, size=3.1) +
  scale_fill_manual(values = cores_2) +
  guides(color = "none")+
  scale_x_discrete(labels = str_wrap(
    c("Total de vítimas", "Homicídio doloso", "Latrocínio", "Lesão corporal seguida de morte", 
    "Mortos pela Polícia Civil e Militar em serviço", "Mortos pela Polícia Civil e Militar fora de 
    serviço"), width = 24))+
  theme_sdpa_let +
  coord_flip(ylim=c(100, 2300))

g <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
              textGrob("Letalidade Violenta", x = 0.03, hjust = 0, gp=gpar(fontsize=30, col="white", 
                                                            fontface="bold")))

grid.arrange(g, p, heights=c(1,9))


# Criar gráfico crimes violentos

p <- base_crimes_long %>% 
  filter(ano_semestre!="2020 / 2º Semestre") %>% 
  ggplot(aes(fill=factor(ano_semestre,  levels=c("2021 / 1º Semestre", "2020 / 1º Semestre")), y= count, 
             x= factor(crime, levels = c(
               "Total", "tot_estupro", "extor_seq", "hd_ocorr", "lat_ocorr", "roubo_veic", 
               "roubo_outros")))) + 
  geom_bar(position="dodge", stat="identity", size=.4, colour="light grey") +
  geom_text(aes(label = count), position = position_dodge(0.94), 
            vjust = 0.43, hjust = -0.5,check_overlap = TRUE, size=3.1) +
  scale_fill_manual(values = cores_2) +
  guides(color = "none")+
  scale_x_discrete(labels = str_wrap(
    c("Total de ocorrências", "Estupro", "Extorsão mediante sequestro", "Homicídio doloso", 
      "Latrocínio", "Roubo de veículo", "Roubo (outros)"), width = 24))+
  theme_sdpa_let +
  coord_flip(ylim=c(100, 145000))

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
        aspect.ratio=8.5/20)

cores <- c("#cec8c4", "#be9068","#042e3f")

# Criar gráfico crimes por ano_semestre/macroregião

grafico_semestre <- function(crime, titulo) { #selecionar o tipo de crime e titulo
                                              # do gráfico

p <- base_crimes %>% 
  filter(cod_reg<31) %>% 
  ggplot(aes(fill=factor(regiao, levels=c("Interior", "Grande São Paulo","Capital")),
             y= {{crime}}, x= ano_semestre)) + 
  geom_bar(position="stack", stat="identity", size=.4, colour="light grey") +
  geom_text(aes(label = {{crime}}, colour =ifelse(cod_reg>11, "black", "white")), 
            position=position_stack(vjust=0.5)) +
  scale_colour_manual(values=c("white"="white", "black"="black")) +
  stat_summary(fun = sum, aes(label = ..y.., group = ano_semestre), 
               geom = "text",size=4, vjust = -0.5) +
  scale_fill_manual(values = cores) +
  guides(color = "none")+
  theme_sdpa_macroreg

g <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
                 textGrob(titulo, x = 0.03, hjust = 0, gp=gpar(fontsize=30, col="white", 
                                          fontface="bold")))

grid.arrange(g, p, heights=c(1,9))

}

grafico_semestre(hd_ocorr, "Prisões")  # Teste da função

# Criar gráfico taxa de crimes por ano_semestre/deinter

theme_sdpa_deinter <- theme_classic()+
  theme(legend.position = "bottom",
        axis.text.y=element_text(size=12),
        legend.title = element_blank(), 
        legend.text=element_text(size=12),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        aspect.ratio=8.7/20)

grafico_deinter <- function(crime, titulo, limite) {  #selecionar o tipo de crime, titulo
                                                      # do gráfico e limite do eixo x
  
p <- base_crimes %>% 
  filter(ano>=(ano_referencia - 1)) %>% 
  filter(cod_reg>30) %>% 
  mutate(taxa = round(({{crime}}/pop * 100000),1)) %>%
  ggplot(aes(fill=ano_semestre, y= taxa, x= deinter)) + 
  geom_bar(position="dodge", stat="identity", size=.4, colour="light grey") +
  geom_text(aes(label = taxa), position = position_dodge(0.94), 
            vjust = 0.43, hjust = -0.5,check_overlap = TRUE, size=3) +
  scale_fill_manual(values = cores) +
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

grafico_deinter(prisoes, "Prisões (taxa por 100 mil habitantes)", 330)

# Criar gráfico letalidade policial

cores_3 <- c("#be9068", "#042e3f")

p <- base_corregedoria_long %>% 
  filter(ano_semestre != "2020 / 2º Semestre") %>% 
  filter(let_vit == "let_ser" | let_vit == "let_fol") %>% 
  ggplot(aes(fill=let_vit, x=ano_semestre, y= count))+
  geom_bar(position="stack", stat="identity", size=.4, colour="light grey") +
  geom_text(aes(label = count, colour =ifelse(let_vit == "let_fol", "black", "white")), 
            position=position_stack(vjust=0.5)) +
  scale_colour_manual(values=c("white"="white", "black"="black")) +
  stat_summary(fun = sum, aes(label = ..y.., group = ano_semestre), 
               geom = "text",size=4, vjust = -0.5) +
  guides(color = "none")+
  scale_fill_manual(labels = c("Letalidade fora de serviço", "Letalidade em serviço"), 
                    values = cores_3) +
  theme_sdpa_macroreg

g <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
              textGrob("Letalidade Policial", x = 0.03, hjust = 0, 
                       gp=gpar(fontsize=22, col="white", fontface="bold")))

grid.arrange(g, p, heights=c(1,9))

# Criar gráfico vitimização policial

p <- base_corregedoria_long %>% 
  filter(ano_semestre != "2020 / 2º Semestre") %>% 
  filter(let_vit == "mort_ser" | let_vit == "mort_fol") %>% 
  ggplot(aes(fill=let_vit, x=ano_semestre, y= count))+
  geom_bar(position="stack", stat="identity", size=.4, colour="light grey") +
  geom_text(aes(label = count, colour =ifelse(let_vit == "mort_fol", "black", "white")), 
            position=position_stack(vjust=0.5)) +
  scale_colour_manual(values=c("white"="white", "black"="black")) +
  stat_summary(fun = sum, aes(label = ..y.., group = ano_semestre), 
               geom = "text",size=4, vjust = -0.5) +
  guides(color = "none")+
  scale_fill_manual(labels = c("Policiais mortos fora de serviço", "Policiais mortos em serviço"), 
                    values = cores_3) +
  scale_y_continuous(limits = c(0, 40))+
  theme_sdpa_macroreg

g <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
              textGrob("Vitimização Policial", x = 0.03, hjust = 0, 
                       gp=gpar(fontsize=22, col="white", fontface="bold")))

grid.arrange(g, p, heights=c(1,9))

# Criar gráficos violencia contra mulher

grafico_viol_mulher <- function(crime, titulo) { #selecionar o tipo de crime e titulo
  # do gráfico
  
  p <- base_viol_mulher_long %>% 
    ggplot(aes(fill=factor(area, levels=c("Interior","Grande São Paulo","Capital")),
               y= {{crime}}, x= ano_semestre)) + 
    geom_bar(position="stack", stat="identity", size=.4, colour="light grey") +
    geom_text(aes(label = {{crime}}, colour =ifelse(area == "Capital",  "white", "black")),
              position=position_stack(vjust=0.5)) +
    scale_colour_manual(values=c("white"="white", "black"="black")) +
    stat_summary(fun = sum, aes(label = ..y.., group = ano_semestre), 
                 geom = "text",size=4, vjust = -0.5) +
    scale_fill_manual(values = cores) +
    guides(color = "none")+
    theme_sdpa_macroreg
  
  g <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
                textGrob(titulo, x = 0.03, hjust = 0, gp=gpar(fontsize=20, col="white", 
                                                              fontface="bold")))
  
  grid.arrange(g, p, heights=c(1,9))
  
}

grafico_viol_mulher (`LESÃO CORPORAL DOLOSA`, "Violência Contra a Mulher: Lesão Corporal Dolosa")  # Teste da função


# Criar tabela letalidade e vitimização policial 
# (ALERTA DE GAMBIARRA: NÃO TERMINAMOS A TABELA, INSERIR COMO IMAGEM NO BOLETIM 2021 1 SEM)

base_corregedoria_estado <- base_corregedoria %>% 
  filter(ano_semestre == "2020 / 1º Semestre"|ano_semestre == "2021 / 1º Semestre") %>% 
  select(cod_reg, ano_semestre, let_ser,let_fol,mort_ser,mort_fol) %>% 
  filter(cod_reg!="30") %>%
  group_by(ano_semestre) %>% 
  summarise(let_ser = sum(let_ser),
            let_fol = sum(let_fol),
            mort_ser = sum(mort_ser),
            mort_fol = sum(mort_fol)) %>% 
  pivot_longer(
    cols = ano_semestre,
    names_to = "região",
    values_to = "ano_semestre"
  )
base_corregedoria_estado$região <- "Estado"

base_corregedoria_capital <- base_corregedoria %>% 
  filter(ano_semestre == "2020 / 1º Semestre"|ano_semestre == "2021 / 1º Semestre") %>% 
  select(cod_reg, ano_semestre, let_ser,let_fol,mort_ser,mort_fol) %>% 
  filter(cod_reg=="10") %>%
  group_by(ano_semestre) %>% 
  summarise(let_ser = sum(let_ser),
            let_fol = sum(let_fol),
            mort_ser = sum(mort_ser),
            mort_fol = sum(mort_fol)) %>% 
  pivot_longer(
    cols = ano_semestre,
    names_to = "região",
    values_to = "ano_semestre"
  )

base_corregedoria_capital$região <- "Capital"

base_corregedoria_tabela <- rbind(base_corregedoria_estado,base_corregedoria_capital)

base_corregedoria_tabela <- base_corregedoria_tabela %>% 
  group_by(região) %>% 
  mutate(razao_ser = let_ser/mort_ser) %>% 
  mutate(razao_fol = let_fol/mort_fol) %>% 

base_corregedoria_tabela%>% 
  knitr::kable()

# Criar módulo de títulos sem os gráficos anexos


titulo <- function(titulo) { #selecionar o titulo

  t <- grobTree(rectGrob(gp=gpar(fill="#042e3f")),
                textGrob(titulo, x = 0.03, hjust = 0, gp=gpar(fontsize=30, col="white", 
                                                              fontface="bold")))
  
  grid.arrange(t, heights=c(1,9), clipGrob(1,1))
  
}

titulo("Destaques") 

#### O que falta ----

# Escrever Destaques
# Escrever Considerações Finais

# Inserir numeração das pgs (.rmd)

# aumentar espaçamento texto apresentaçào e destaque (.rmd)
# aumentar o espaçamento entre as barras dos gráficos

## inserir variação porcentagens
