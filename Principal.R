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
    departa == "Deinter 01" ~ 31,
    departa == "Deinter 02" ~ 32,
    departa == "Deinter 03" ~ 33,
    departa == "Deinter 04" ~ 34,
    departa == "Deinter 05" ~ 35,
    departa == "Deinter 06" ~ 36,
    departa == "Deinter 07" ~ 37,
    departa == "Deinter 08" ~ 38,
    departa == "Deinter 09" ~ 39,
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
            mort_fol = sum(mort_fol))

saveRDS(base_crimes, "base_crimes.rds") 
saveRDS(base_corregedoria, "base_corregedoria.rds") 


###Passo 07: Criando os gráficos----

# Criar tema SDPA

theme_sdpa_macroreg <- theme_void()+
  theme(legend.position = "bottom",
        axis.text.x=element_text(size=10),
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
  geom_bar(position="stack", stat="identity", size=.3, colour="black") +
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

grafico_semestre(prisoes, "Prisões")  # Teste da função

# Criar gráfico taxa de crimes por ano_semestre/deinter

theme_sdpa_deinter <- theme_classic()+
  theme(legend.position = "bottom",
        axis.text.y=element_text(size=11),
        legend.title = element_blank(), 
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
  geom_bar(position="dodge", stat="identity", size=.3, colour="black") +
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

grafico_deinter(prisoes, "Prisões (taxa por 100 mil habitantes)", 310)





