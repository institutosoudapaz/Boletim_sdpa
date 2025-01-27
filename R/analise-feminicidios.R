library(tidyverse)

# Importar dados ----------------------------------------------------------

SIPCV_2023 <- readxl::read_excel("data-raw/SIPCV_2024.xlsx", 
                         col_types = c("text", "text", "text", 
                                       "text", "text", "numeric", "text", 
                                       "text", "date", "date", "text", "date", 
                                       "text", "text", "text", "text", "text", 
                                       "text", "text", "text", "text", "text", 
                                       "text", "text", "text", "text", "text", 
                                       "text", "text", "text", "text", "text", 
                                       "text", "text", "numeric", "text", 
                                       "text", "text", "text", "text", "text", 
                                       "text", "text", "text", "text", "text", 
                                       "numeric", "date", "date", "text", 
                                       "text", "text", "text")) |>
  janitor::clean_names()

SIPCV_2024 <- readxl::read_excel("data-raw/SIPCV_2024.xlsx", sheet = 2,
                                 col_types = c("text", "text", "text", 
                                               "text", "text", "numeric", "text", 
                                               "text", "date", "date", "text", "date", 
                                               "text", "text", "text", "text", "text", 
                                               "text", "text", "text", "text", "text", 
                                               "text", "text", "text", "text", "text", 
                                               "text", "text", "text", "text", "text", 
                                               "text", "text", "numeric", "text", 
                                               "text", "text", "text", "text", "text", 
                                               "text", "text", "text", "text", "text", 
                                               "numeric", "date", "date", "text", 
                                               "text", "text", "text")) |>
  janitor::clean_names()

# Filtra os casos de feminicídios tentados e consumados

femi_2023 <- SIPCV_2023 |> 
  dplyr::filter(detalhamento_do_homicidio_doloso == "FEMINICIDIO" | 
                  detalhamento_do_homicidio_doloso == "FEMINICIDIO TENTADO")

femi_2024 <- SIPCV_2024 |> 
  dplyr::filter(detalhamento_do_homicidio_doloso == "FEMINICIDIO" | 
                  detalhamento_do_homicidio_doloso == "FEMINICIDIO TENTADO")

# junta as bases
                
femi_2023_2024 <- rbind(femi_2023, femi_2024)


# Tratar os dados para análise --------------------------------------------

# Idade
femi_2023_2024 <- femi_2023_2024 |>  
  mutate(
    idade_agrupado = case_when(
      idade_data_ocorrencia < 16 ~ "12 a 15 anos",
      idade_data_ocorrencia >= 16 & idade_data_ocorrencia < 19 ~ "16 a 18 anos",
      idade_data_ocorrencia >= 18 & idade_data_ocorrencia < 30 ~ "18 a 29 anos",
      idade_data_ocorrencia >= 30 & idade_data_ocorrencia < 40 ~ "30 a 39 anos",
      idade_data_ocorrencia >= 40 & idade_data_ocorrencia < 50 ~ "40 a 49 anos",
      idade_data_ocorrencia >= 50 & idade_data_ocorrencia < 60 ~ "50 a 59 anos",
      idade_data_ocorrencia >= 60 & idade_data_ocorrencia < 99 ~ "60 anos ou mais",
      TRUE ~ "Não informado"
    )
  )

# Raça/cor

femi_2023_2024 <- femi_2023_2024 |>  
  mutate(
    cor_limpo = case_when(
      cor_curtis %in% c("Branca", "BRANCA") ~ "Branca",
      cor_curtis %in% c("Preta", "PRETA", "Preta ", "Preta.") ~ "Preta",
      cor_curtis %in% c("Parda", "PARDA", "parda") ~ "Parda",
      cor_curtis %in% c("Amarela") ~ "Amarela",
      TRUE ~ "Não informado"
    )
  )

# Instrumento/Arma

femi_2023_2024 <- femi_2023_2024 |>  
  mutate(
    instrumento_limpo = case_when(
      possivel_meio_utilizado %in% c("MEIOS NAO ESPECIFICADOS", "NAO IDENTIFICADO",
                                     "OUTROS MEIOS NAO ESPECIFICADOS", "NULL", NULL) 
      ~ "Não especificado",
      possivel_meio_utilizado %in% c("DISPARO ARMA DE FOGO DE MAO", 
                                     "DISPARO DE ESPINGARDA, CARABINA OU ARMA DE FOGO DE MAIOR CALIBRE",
                                     "DISPARO DE OUTRA ARMA DE FOGO E DE ARMA DE FOGO NAO ESPECIFICADA") 
      ~ "Arma de fogo",
      possivel_meio_utilizado %in% c("OBJETO CORTANTE OU PENETRANTE") 
      ~ "Objeto cortante ou penetrante",
      possivel_meio_utilizado %in% c("ENFORCAMENTO, ESTRANGULAMENTO E SUFOCAÇAO",
                                     "FORÇA CORPORAL") 
      ~ "Enforcamento/Força corporal",
      possivel_meio_utilizado %in% c("OBJETO CONTUNDENTE")
      ~ "Objeto contundente",
      TRUE ~ "Outros"
    )
  )

# Tipo/local de ocorrência

femi_2023_2024 <- femi_2023_2024 |>  
  mutate(
  local_limpo = case_when(
    descr_tipolocal %in% c("Casa", "Residência", "Apartamento", "Casas", 
                           "Condomínio Residencial", "RESIDENCIA", 
                           "Apartamentos", "Moradia", "CONDOMINIO RESIDENCIAL",
                           "CondomInio Residencial", "Residências")
    ~ "Residência",
    descr_tipolocal %in% c("Via Pública", "Rodovia/Estrada", "Acostamento",
                           "De Frente a Residência da Vítima",
                           "Interior de Veículo Particular", "Favela",
                           "VIA PUBLICA", "Praça", "RODOVIA/ESTRADA",
                           "Rodoviário", "Semáforo", "Túnel/Viaduto/Ponte",
                           "Veículo em movimento", "Viela", "Ônibus/Lotação/Trolebus") 
    ~ "Via Pública",
    descr_tipolocal %in% c("Unidade Rural", "Sítio", "Chácara", "Fazenda", 
                           "Chácaras") 
    ~ "Zona rural",
    descr_tipolocal %in% c("Hospital", "Saúde", "Posto de Saúde", "SAUDE") 
    ~ "Hospital/unidade de saúde",
    descr_tipolocal %in% c("Restaurante e Afins", "Bar/Botequim", "Mercado", 
                           "Comércio e Serviços", "Bar", "Café/Lanchonete",
                           "Conveniência", "Lanchonete/Pastelaria/Pizzaria",
                           "Motel", "Restaurante", "Salão de Beleza/Estética",
                           "Casa Noturna/Outros", "Condomínio Comercial",
                           "Doceria/Bomboniere/Sorveteria", "Farmácia/Drogaria",
                           "Lojas", "Posto de Gasolina", "Shopping Center")
    ~ "Comércio",
    TRUE ~ "Outros"
  )
)

#limpa hora 
femi_2023_2024 <- femi_2023_2024 |> mutate(hora = str_sub(hora_ocorrencia_bo,  start = 12))

# criar uma nova variável para o turno (manhã, tarde, noite, madrugada) baseada na coluna "hora"

femi_2023_2024 <- femi_2023_2024 |> mutate(
  periodo = case_when(
  is.na(hora) ~ NA_character_,
  hora <= "05:59" ~ "Madrugada",
  hora <= "11:59" ~ "Manhã",
  hora <= "17:59" ~ "Tarde",
  TRUE ~ "Noite"
  )
)

saveRDS(femi_2023_2024, "data-raw/femi_2023_2024.rds")
writexl::write_xlsx(femi_2023_2024, "data-raw/femi_2023_2024.xlsx")
