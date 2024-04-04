# Carregar os pacotes -------------------------------------------------------------------------

library(tidyverse)

# Carregar as bases e funções -----------------------------------------------------------------

base_anual_dp <- readxl::read_xlsx("./data-raw/base_17_23_dp_capital.xlsx")

# Arquivo shape filtrando a capital
shp_capital <- sf::st_read("./data-raw/shapes/Distrito_policial_SP.shp", quiet = TRUE) |>  
  filter(DepGeoDes == "DECAP")

# Carrega funções de limpeza do nome dos DPs
source("./R/funcoes/limpeza_dp.R")

# Tratar o shape para criação do mapa ---------------------------------------------------------

# Limpar DPs
shp_capital$DpGeoDes <- limpeza_dp(shp_capital$DpGeoDes)

# Mesclar base_mensal e o shape pela coluna de nome dos DPs

base_dp <- base_anual_dp %>% 
  #filtra ano de interesse
  filter(cod_ano == "2023") %>% 
  filter(cod_reg == 10) %>% 
  right_join(shp_capital, by = c("nom_del" = "DpGeoDes")) %>% 
  mutate(legenda = stringr::str_extract(nom_del,"^.{3}"))

# Temas e cores dos mapas ---------------------------------------------------------------------

theme_sdpa_maps <- theme_void()+
  theme(legend.text=element_text(size=10),
        legend.title=element_blank (),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.key.size = unit(0.5, 'cm'), 
        plot.margin=unit(c(0.2,0,0,0), 'cm'))

cores_mapa <- c("#F9F9F9", "#D0E4FF", "#99BFEF", "#5295D4", "#0066A5", "#00366C")

# Código manual para criar os mapas -----------------------------------------------------------

# Mapa de DP
mapa <- base_dp |> 
  # somar roubos de veículos e outros
  mutate(total_roubos = roubo_outros + roubo_veic) |> 
  # criar niveis da categoria
  mutate(total_roubos_agregado = case_when(
    total_roubos < 250 ~ "Menos de 250 roubos",
    total_roubos > 250 & total_roubos <= 500 ~ "Entre 251 e 500 roubos",
    total_roubos > 500 & total_roubos <= 750 ~ "Entre 501 e 750 roubos",
    total_roubos > 750 & total_roubos <= 1250 ~ "Entre 751 e 1250 roubos",
    total_roubos > 1250 & total_roubos <= 2000 ~ "Entre 1251 e 2000 roubos",
    total_roubos > 2000 ~ "Mais de 2000 roubos")) |> 
  ggplot() +
  # fill = categoria do crime agregado
  geom_sf(aes(geometry = geometry, fill = total_roubos_agregado))+
  geom_sf_text(aes(geometry = geometry, label =  legenda), size = 1, color = "black") +
  scale_fill_manual(values = cores_mapa, name = NULL, 
                    # mudar nome das categorias crime agregado
                    breaks=c("Menos de 250 roubos", "Entre 251 e 500 roubos", 
                             "Entre 501 e 750 roubos", "Entre 751 e 1250 roubos",
                             "Entre 1251 e 2000 roubos", "Mais de 2000 roubos"))+
  theme_sdpa_maps

ggsave("mapa.svg", plot = mapa, device = "svg", width = 20, height = 20, units = "cm")

# Mapa de Seccional
mapa <- base_dp |> 
  # Agrupar dados do crime por seccional
  group_by(seccional, keep.all=TRUE) |>
  mutate(total_hd_vitima = sum(hd_vitima)) |>
  # criar niveis da categoria
  mutate(hd_vitima_agregado = case_when(
    total_hd_vitima < 40 ~ "Menos de 40 vítimas de homicídios",
    total_hd_vitima > 40 & total_hd_vitima <= 60 ~ "Entre 40 e 60 vítimas de homicídios",
    total_hd_vitima > 60 & total_hd_vitima <= 80 ~ "Entre 61 e 80 vítimas de homicídios",
    total_hd_vitima > 80 & total_hd_vitima <= 110 ~ "Entre 81 e 110 vítimas de homicídios",
    total_hd_vitima > 110 ~ "Mais de 110 vítimas de homicídios")) |> 
  ggplot() +
  # fill = categoria do crime agregado
  geom_sf(aes(geometry = geometry, fill = hd_vitima_agregado))+
  # define tamanho e cor dos rótulos dos DPs/seccionais
  # geom_sf_text(aes(geometry = geometry, label = legenda), size = 1, color = "black") +
  scale_fill_manual(values = cores_mapa, name = NULL, 
                    # mudar nome das categorias crime agregado
                    breaks=c("Menos de 40 vítimas de homicídios", 
                             "Entre 40 e 60 vítimas de homicídios", 
                             "Entre 61 e 80 vítimas de homicídios", 
                             "Entre 81 e 110 vítimas de homicídios",
                             "Mais de 110 vítimas de homicídios")) +
  theme_sdpa_maps 

ggsave("mapa_sec.svg", plot = mapa, device = "svg", width = 20, height = 20, units = "cm")
