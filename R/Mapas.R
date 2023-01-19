
# Dados para futura criação de mapas do SDP Analisa

# Carregar os pacotes -------------------------------------------------------------------------

library(tidyverse)
#library(ggplot2)
#library(stringr)
#library(tidyr)
#library(rvest)
#library(sf)

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
    scale_fill_steps((scales::breaks_extended(n=6)), labels = scales::label_comma(), 
                     name = "Taxa por \n100 mil hab.", 
                     guide = guide_colourbar(barheight = unit(4.5, "cm"), barwidth = unit(0.8, "cm")),
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
