
# Carregar pacotes e base mensal ------------------------------------------

library(tidyverse)
library(ggplot2)

base_mensal_23 <- read.csv2("../isdp_scraper/output/base_mensal/base_mensal_v7_2023.csv")
base_mensal_22 <- read.csv2("../isdp_scraper/output/base_mensal/base_mensal_v7_2022.csv")

base_mensal <- rbind(base_mensal_23, base_mensal_22)

# Abrir arquivo shape
shp_capital <- sf::st_read("./data-raw/shapes/Distrito_policial_SP.shp", quiet = TRUE) |>  
  filter(DepGeoDes == "DECAP")

# Temas dos mapas
theme_sdpa_maps <-  theme_void()+
  theme(legend.text=element_text(size=10),
        legend.title=element_blank (),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.key.size = unit(0.5, 'cm'), 
        plot.margin=unit(c(0.2,0,0,0), 'cm'))

colors_map <- c("#F9F9F9", "#D0E4FF", "#99BFEF", "#5295D4", "#0066A5", "#00366C")

# Modelagem base mensal  --------------------------------------------------

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

# Escolha o modelo adequado:
# 1 para tri01; 2 para Semestre; 3 para tri03 e 4 para Anual

modelo <- 2

ano_referencia <- 2023

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

# Criar a base mensal por DP

base_mensal_dp <- base_mensal %>% 
  group_by(periodo, nom_del) %>% 
  summarise(hd_vitima = sum(o02, na.rm=TRUE),
            hd_ocorr = sum(o01, na.rm=TRUE),
            lat_ocorr = sum(o12, na.rm=TRUE),
            lat_vitima =sum(o13, na.rm=TRUE),
            tot_estupro =sum(o14, na.rm=TRUE),
            estupro_vuln =sum(o16, na.rm=TRUE),
            roubo_outros =sum(o18, na.rm=TRUE),
            roubo_veic = sum(o19, na.rm=TRUE),
            lesao_morte = sum(o08, na.rm=TRUE),
            ap_armas =sum(p05, na.rm=TRUE),
            prisoes =sum(p11, na.rm=TRUE),
            cod_reg = unique(cod_reg))

# Modelagem para criação dos mapas ----------------------------------------

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

# Mesclar base_mensal e o shape pela coluna de nome dos DPs

tab_dp <- base_mensal_dp %>% 
  filter(periodo %in% c("2022/1º Semestre", "2023/1º Semestre")) %>% 
  filter(cod_reg == 10) %>% 
  right_join(shp_capital, by = c("nom_del" = "DpGeoDes")) %>% 
  mutate(legenda = stringr::str_extract(nom_del,"^.{3}"))


# Mapas -------------------------------------------------------------------

# Roubos
tab_dp |> 
  filter(periodo == "2023/1º Semestre") |> 
  mutate(total_roubos = roubo_outros + roubo_veic) |> 
  mutate(total_roubos_agregado = case_when(
    total_roubos < 500 ~ "Menos de 500 roubos",
    total_roubos > 500 & total_roubos <= 1000 ~ "Entre 501 e 1000 roubos",
    total_roubos > 1000 & total_roubos <= 1500 ~ "Entre 1001 e 1500 roubos",
    total_roubos > 1500 & total_roubos <= 2000 ~ "Entre 1501 e 2000 roubos",
    total_roubos > 2000 & total_roubos <= 3000 ~ "Entre 2001 e 3000 roubos",
    total_roubos > 3000 ~ "Mais de 3000 roubos")) |> 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = total_roubos_agregado))+
  geom_sf_text(aes(geometry = geometry, label =  legenda), size = 1, color = "black") +
  scale_fill_manual(values = colors_map, name = NULL, 
                    breaks=c("Menos de 500 roubos", "Entre 501 e 1000 roubos",
                             "Entre 1001 e 1500 roubos", "Entre 1501 e 2000 roubos",
                             "Entre 2001 e 3000 roubos", "Mais de 3000 roubos"))+
  theme_sdpa_maps

# Homicídios
tab_dp |> 
  mutate(hd_vitima_agregado = case_when(
    hd_vitima < 1 ~ "Sem homicídios no período",
    hd_vitima == 1 ~ "1 vítima de homicídio",
    hd_vitima == 2 ~ "2 vítimas de homicídio",
    hd_vitima > 2 & hd_vitima <= 5 ~ "Entre 3 e 5 vítimas de homicídio",
    hd_vitima > 5 & hd_vitima <= 9 ~ "Entre 6 e 9 vítimas de homicídio",
    hd_vitima > 9 ~ "Mais de 10 vítimas de homicídio")) |> 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = hd_vitima_agregado))+
  geom_sf_text(aes(geometry = geometry, label =  legenda), size = 1, color = "black") +
  scale_fill_manual(values = colors_map, name = NULL, 
                    breaks=c("Sem homicídios no período","1 vítima de homicídio",
                             "2 vítimas de homicídio",
                             "Entre 3 e 5 vítimas de homicídio",
                             "Entre 6 e 9 vítimas de homicídio",
                             "Mais de 10 vítimas de homicídio"))+
  theme_sdpa_maps +
  facet_wrap(~periodo)

# Estupros
tab_dp |> 
  filter(periodo == "2023/1º Semestre") |> 
  mutate(prop_estupro = (estupro_vuln / tot_estupro) * 100) |> 
  mutate(prop_estupro_agregado = case_when(
    prop_estupro <= 40 ~ "Até 40% do total",
    prop_estupro > 4 & prop_estupro <= 50 ~ "41% a 50%",
    prop_estupro > 50 & prop_estupro <= 60 ~ "51% a 60%",
    prop_estupro > 60 & prop_estupro <= 70 ~ "61% a 70%",
    prop_estupro > 70 & prop_estupro <= 90 ~ "71% a 90%",
    prop_estupro > 90 ~ "91% ou mais do total")) |> 
  ggplot() +
  geom_sf(aes(geometry = geometry, fill = prop_estupro_agregado))+
  geom_sf_text(aes(geometry = geometry, label =  legenda), size = 1, color = "black") +
  scale_fill_manual(values = colors_map,
                    breaks=c("Até 40% do total", "41% a 50%", "51% a 60%", 
                             "61% a 70%","71% a 90%","91% ou mais do total"))+
  theme_sdpa_maps 
