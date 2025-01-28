library(tidyverse)
library(janitor)

# Importa os dados --------------------------------------------------------

base <- readxl::read_excel("./data-raw/MPSP_2017-2025.xlsx")

#limpa a base 
base <- base %>%  janitor::clean_names()

view(base)

# Tratamento e criação de colunas -----------------------------------------

#cria o ano 
base <- base |> mutate(ano = str_sub(data, end = 4))

#limpa hora 
base <- base |> mutate(hora = str_sub(hora,  start = 12))

# Criar uma nova coluna para períodos com base no ano
if ("ano" %in% names(base)) {
  base <- base %>% 
    mutate(periodo = case_when(
      ano %in% c(2019, 2020) ~ "Pré Câmeras",
      ano %in% c(2021, 2022) ~ "Câmeras Implementadas",
      ano %in% c(2023, 2024) ~ "Gestão Tarcísio",
      TRUE ~ "Outro"
    ))
} else {
  stop("A variável 'ano' não está presente na base 'base'.")
}

# Padronizar e reclassificar as variáveis da coluna 'forca'
base <- base %>%
  mutate(
    forca_limpo = case_when(
      forca %in% c("Polícia Militar", "Polícia Militar - GCM", "Polícia Militar-GCM", "Policia Militar","Polícia Civil-Polícia Militar", "Polícia Civil - Polícia Militar") ~ "Polícia Militar",
      forca %in% c("Polícia Civil", "Polícia Civil - GCM", "Polícia Civil-GCM") ~ "Polícia Civil",
      forca %in% c("GCM") ~ "Guarda Civil Municipal (GCM)",
      forca %in% c("Polícia Penal") ~ "Polícia Penal",
      forca %in% c("Polícia Federal") ~ "Polícia Federal",
      forca %in% c("Polícia Rodoviária Federal", "PRF") ~ "Polícia Rodoviária Federal",
      forca %in% c("SUPSEG-Fundação Casa-SP") ~ "SUPSEG Fundação Casa-SP",
      forca %in% c("Aeronáutica") ~ "Aeronáutica",
      TRUE ~ forca  # Caso não esteja listado, mantém o valor original
    )
  ) 

# Padronizar e reclassificar as variáveis da coluna 'regiao_adm'
base <- base %>%
  mutate(
    regiao_adm_limpa = case_when(
      regiao_adm %in% c("Capital", "CAPITAL") ~ "Capital",
      regiao_adm %in% c("GSP") ~ "GSP",
      regiao_adm %in% c("DEINTER 1", "Deinter 1", "DEITNER 1") ~ "DEINTER 1",
      regiao_adm %in% c("DEINTER 2", "Deinter 2") ~ "DEINTER 2",
      regiao_adm %in% c("DEINTER 3", "Deinter 3") ~ "DEINTER 3",
      regiao_adm %in% c("DEINTER 4", "Deinter 4") ~ "DEINTER 4",
      regiao_adm %in% c("DEINTER 5", "Deinter 5", "deinter 5") ~ "DEINTER 5",
      regiao_adm %in% c("DEINTER 6", "Deinter 6", "DeInter 6", "DIENTER 6", "DENTER 6") ~ "DEINTER 6",
      regiao_adm %in% c("DEINTER 7", "Deinter 7", "DEINTER  7", "DEITNER 7") ~ "DEINTER 7",
      regiao_adm %in% c("DEINTER 8", "Deinter 8") ~ "DEINTER 8",
      regiao_adm %in% c("DEINTER 9", "Deinter 9") ~ "DEINTER 9",
      regiao_adm %in% c("DEINTER 10", "Deinter 10") ~ "DEINTER 10",
      regiao_adm %in% c("2ª Central Estadual do Plantão Digital/SIPJ") ~ "2ª Central Estadual do Plantão Digital/SIPJ",
      regiao_adm %in% c("ESTADO DO MATO GROSSO DO SUL") ~ "Estado do Mato Grosso do Sul",
      TRUE ~ regiao_adm  
    )
  )

# região administrativa
base %>% group_by(regiao_adm_limpa) %>% count() %>%  view()

#Filtra Ocorrências em Serviço da PM
base_limpa <-  base %>%  filter(servico == "SIM" & forca_limpo == "Polícia Militar")

#Fitra anos 2019-2024
base_limpa <- base_limpa %>%  filter(ano >= 2019 & ano <= 2024)

#Analisa por período o total de ocorrências, batalhão, bairros 
base_limpa %>% filter(cidade=="SÃO PAULO") %>%  group_by(batalhao,periodo) %>%  count() %>%  view()
base_limpa %>% group_by(batalhao) %>%  count() %>%  view()

#CRIA BASE DE CASOS DA ROTA
base_rota <-  base_limpa %>%  filter(batalhao =="1º BPCh - ROTA")

#casos da rota por ano 
base_rota %>% group_by(ano) %>% count() %>%  view()

#casos da rota por periodo
base_rota %>% group_by(periodo) %>% count() %>%  view()

#Total de casos da Rota por cidade 
base_rota %>% group_by(cidade) %>% count() %>%  view()

#bairros em SP, Santos e Guarujá(top 3 cidades com mais MDIPS da Rota)
#SP
base_rota %>% filter(cidade =="SÃO PAULO") %>% group_by(bairro) %>% count() %>%  view()
#Santos
base_rota %>% filter(cidade =="SANTOS") %>% group_by(bairro) %>% count() %>%  view()
#Guarujá
base_rota %>% filter(cidade =="GUARUJÁ") %>% group_by(bairro) %>% count() %>%  view()

# região administrativa (PM em Serviço)
base_limpa %>% group_by(regiao_adm_limpa) %>% count() %>%  view()


# Mesclar com base de MDIP da SSP-SP --------------------------------------
#Merge das bases *base ssp não contém os dados de dezembro


#abre a base de MDIPs da SSP 
ssp <- readxl::read_excel("./data-raw/MDIP_2024.xlsx",
                  sheet = "MDIP_2013_A_NOV24", 
                  col_types = c("text","text", "text", "text", "text", "text",
                                "text", "numeric", "numeric", "date",
                                "text", "numeric", "text", "text", "text", 
                                "text", "date", "date", "text", "text", "text", 
                                "numeric", "numeric", "text", "text", "text", 
                                "text", "text", "text", "text"))
#limpa a base da ssp
ssp <- ssp %>%  janitor::clean_names()

colnames(ssp)

#deixa os municipios em forma de título
ssp$municipio_circunscricao <- str_to_title(ssp$municipio_circunscricao)
base_limpa$cidade <- str_to_title(base_limpa$cidade)

#função para limpeza de municipios (rodar função em script a parte antes)
ssp <- ssp |>
  mutate(municipio_limpo = limpeza_municipio(municipio_circunscricao))

base_limpa<- base_limpa |>
  mutate(municipio_limpo = limpeza_municipio(cidade))

#cria o ano 
ssp <- ssp |> mutate(ano = str_sub(data_fato, end = 4))
#limpa hora 
ssp <- ssp |> mutate(hora = str_sub(hora_fato,  start = 12))

#filtrar base ssp pelo período analisado (2019-2024)
ssp <- ssp %>% filter(ano_estatistica >=2019 & ano_estatistica<= 2024)

#filtrar base ssp por coorporação e em serviço
ssp <- ssp %>% filter(coorporacao =="PM" & situacao == "Serviço")

#Cria coluna para join
ssp <- ssp |> mutate(var_join = paste(data_fato, hora, municipio_limpo, sep ="/" )) 

base_limpa <- base_limpa|>  mutate(var_join = paste(data, hora, municipio_limpo, sep ="/" ))

#junta bases - usando left_join para priorizar a base da SSP, que tem mais dados

mdip_unificada <- left_join(ssp, base_limpa, by = join_by(var_join))

# Retira as linhas duplicadas

mdip <- mdip_unificada |> 
  distinct(.keep_all = TRUE)

# Comparar número de casos por ano das três bases

ssp |> group_by(ano) |> count() |> view()
base_limpa |> group_by(ano) |> count() |> view()
#base unificada
mdip |> group_by(ano.x) |> count() |> view()

# Salvar base unificada

saveRDS(mdip, "./data-raw/mdip_unificada.rds")


# Análises básicas --------------------------------------------------------

# Total de ocorrências por ano
# criar tabela de número de ocorrências da base mdip por ano

mdip %>%
  group_by(ano.x) %>%
  count() 

base_limpa %>%
  group_by(periodo) %>%
  count() 

mdip %>%
  group_by(periodo) %>%
  count() 

## Gráfico de linha por mês

#faça um gráfico de linhas usando a base mdip de ocorrências por mês

mdip %>%
  group_by(mes_ano) %>%
  count() %>%
  ggplot(aes(x = mes_ano, y = n)) +
  geom_line() +
  labs(title = "Número de ocorrências por mês",
       x = "Mês",
       y = "Número de ocorrências")

# Idade das vítimas

mdip %>%
  group_by(cor_pele) %>%
  count() |> view()

mdip <- mdip |>  
  mutate(
    idade_agrupado = case_when(
      idade_pessoa < 16 ~ "12 a 15 anos",
      idade_pessoa >= 16 & idade_pessoa < 19 ~ "16 a 18 anos",
      idade_pessoa >= 18 & idade_pessoa < 30 ~ "18 a 29 anos",
      idade_pessoa >= 30 & idade_pessoa < 40 ~ "30 a 39 anos",
      idade_pessoa >= 40 & idade_pessoa < 50 ~ "40 a 49 anos",
      idade_pessoa >= 50 & idade_pessoa < 60 ~ "50 a 59 anos",
      idade_pessoa >= 60 & idade_pessoa < 99 ~ "60 anos ou mais",
      TRUE ~ "Não informado"
    )
  )

mdip_idade_ano 
  mdip |> 
    select(ano.x, idade_agrupado) |>
    pivot_wider(names_from = ano.x, values_from = ano.x, values_fn = length, values_fill = 0) |>
    writexl::write_xlsx("./data-raw/mdip_idade_ano.xlsx")

  mdip_idade_ano 
  mdip |> 
    select(periodo, idade_agrupado) |>
    pivot_wider(names_from = periodo, values_from = periodo, values_fn = length, values_fill = 0)

# Raça das vítimas

  mdip %>%
    group_by(idade_pessoa) %>%
    count() |> view()
  
  mdip <- mdip |>  
    mutate(
      cor_pele_limpo = case_when(
        cor_pele %in% c("Branca", "BRANCA") ~ "Branca",
        cor_pele %in% c("Preta", "PRETA", "Preta ", "Preta.") ~ "Preta",
        cor_pele %in% c("Parda", "PARDA", "parda") ~ "Parda",
        TRUE ~ "Não informado"
      )
    )
  
  mdip |> 
    select(ano.x, cor_pele_limpo) |>
    pivot_wider(names_from = ano.x, values_from = ano.x, values_fn = length, values_fill = 0) |>
    writexl::write_xlsx("./data-raw/mdip_cor_ano.xlsx")
  
# Deinter

  mdip %>%
    select(ano.x, departamento_circunscricao) |> 
    pivot_wider(names_from = ano.x, values_from = ano.x, values_fn = length, values_fill = 0) |>
  writexl::write_xlsx("./data-raw/mdip_deinter_ano.xlsx")
  
  
# Cidades

  mdip %>%
    select(ano.x, municipio_circunscricao) |> 
    pivot_wider(names_from = ano.x, values_from = ano.x, values_fn = length, values_fill = 0) |>
    writexl::write_xlsx("./data-raw/mdip_cidade_ano.xlsx")
  
  
  # Distritos policiais SP
  
  mdip %>%
    filter(municipio_circunscricao == "S.paulo") %>%
    select(ano.x, dp_circunscricao) |> 
    pivot_wider(names_from = ano.x, values_from = ano.x, values_fn = length, values_fill = 0) |>
    writexl::write_xlsx("./data-raw/mdip_dp_sp_ano.xlsx")
  
  
# Batalhões

  mdip %>%
    select(ano.x, batalhao, cor_pele) |> 
    pivot_wider(names_from = ano.x, values_from = ano.x, values_fn = length, values_fill = 0) |>
    writexl::write_xlsx("./data-raw/mdip_batalhao_ano.xlsx")
  
## Gráfico de linha por mês dos batalhões que mais matam 


# Mapas -------------------------------------------------------------------

# Arquivo shape filtrando a capital
  shp_capital <- sf::st_read("./data-raw/shapes/Distrito_policial_SP.shp", quiet = TRUE) |>  
    filter(DepGeoDes == "DECAP")
  
  # Carrega funções de limpeza do nome dos DPs
  source("./R/funcoes/limpeza_dp.R")
  
  # Tratar o shape para criação do mapa ---------------------------------------------------------
  
  # Limpar DPs
  shp_capital$DpGeoDes <- limpeza_dp(shp_capital$DpGeoDes)
  ssp$dp_circunscricao <- limpeza_dp(ssp$dp_circunscricao)
  
  # Mesclar base_mensal e o shape pela coluna de nome dos DPs
  
ssp_2024 <- ssp |> 
    #filtra ano de interesse
    #filter(ano == "2024") |> 
    filter(municipio_circunscricao == "S.PAULO") %>%
    left_join(shp_capital, by = c("dp_circunscricao" = "DpGeoDes")) %>% 
    mutate(legenda = stringr::str_extract(dp_circunscricao,"^.{3}")) |> 
   mutate(vitima = case_when(
    ano == 2024 ~ 1,
    TRUE ~ 0))
  
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
  mapa <- ssp_2024 |> 
    group_by(dp_circunscricao, ano) |>
    mutate(vitima = sum(vitima)) |> 
    # criar niveis da categoria
    mutate(total_mdip_agregado = case_when(
      vitima < 1 ~ "Sem MDIP no ano",
      vitima >= 1 & vitima < 2 ~ "1 morte no ano",
      vitima > 1 & vitima <= 2 ~ "2 mortes no ano",
      vitima > 2 & vitima <= 4 ~ "Entre 3 e 4 mortes no ano",
      vitima > 4 & vitima <= 6 ~ "Entre 5 e 6 mortes no ano",
      vitima > 6 ~ "Entre 7 e 8 mortes no ano")) |> 
    ggplot() +
    # fill = categoria do crime agregado
    geom_sf(aes(geometry = geometry, fill = total_mdip_agregado))+
    geom_sf_text(aes(geometry = geometry, label =  legenda), size = 1, color = "black") +
    scale_fill_manual(values = cores_mapa, name = NULL, 
                      # mudar nome das categorias crime agregado
                      breaks=c("Sem MDIP no ano", "1 morte no ano", 
                               "2 mortes no ano", "Entre 3 e 4 mortes no ano",
                               "Entre 5 e 6 mortes no ano", "Entre 7 e 8 mortes no ano"))+
    theme_sdpa_maps
  
  ggsave("mapa.jpeg", plot = mapa, device = "jpeg", width = 20, height = 20, units = "cm")
  
# faça um mapa de linha com o número de mortes por mês de 2019 a 2024 usando a base ssp

ssp %>%
  mutate(mes_ano = str_sub(data_fato, end = 7)) |> 
  filter(ano_estatistica >=2019 & ano_estatistica<= 2024) |> 
  group_by(mes_ano) %>%
  count() %>%
  ggplot(aes(x = mes_ano, y = n, group = 1)) +
  geom_line(color="#69b3a2", linewidth=1, alpha=0.9) +
  labs(title = "Número de mortes por mês",
       x = "Mês",
       y = "Número de mortes")+
  theme(axis.text.x = element_text(angle = 90, size=7, face=3))
