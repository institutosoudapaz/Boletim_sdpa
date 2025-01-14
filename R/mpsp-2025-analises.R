#bibliotecas
library(tidyverse)
library(janitor)
#importa a base
base <- read_excel("./data-raw/MPSP_2017-2025.xlsx")

#limpa a base 
base <- base %>%  janitor::clean_names()
view(base)
#cria o ano 
base <- base |> mutate(ano = str_sub(data, end = 4))
#limpa hora 
base <- base |> mutate(hora = str_sub(hora,  start = 12))

#cria periodos 
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
base <- base %>%
  mutate(
    regiao_adm_limpa = case_when(
      regiao_adm %in% c("Capital", "CAPITAL") ~ "Capital",
      regiao_adm %in% c("GSP") ~ "GSP",
      regiao_adm %in% c("DEINTER 1", "Deinter 1", "DEITNER 1") ~ "DEINTER 1",
      regiao_adm %in% c("DEINTER 2", "Deinter 2") ~ "DEINTER 2",
      regiao_adm %in% c("DEINTER 3", "Deinter 3") ~ "DEINTER 3",
      regiao_adm %in% c("DEINTER 4", "Deinter 4") ~ "DEINTER 4",
      regiao_adm %in% c("DEINTER 5", "Deinter 5") ~ "DEINTER 5",
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

#Filtra Corporação 
base_limpa <-  base %>%  filter(forca_limpo == "Polícia Militar")

#Filtra Ocorrências em Serviço
base_limpa <-  base %>%  filter(servico=="SIM")

#Fitra anos 2019-2024
base_limpa <- base %>%  filter(ano >= 2019 & ano <= 2024)

#analisa por período o total de ocorrências, batalhão, bairros 
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

#Merge das bases *base ssp não contém os dados de dezembro

#abre a base de MDIPs da SSP 
ssp <- read_excel("./data-raw/MDIP_2024 (4).xlsx", 
                  +     sheet = "MDIP_2013_A_NOV24", col_types = c("text", 
                                                                            "text", "text", "text", "text", "text", 
                                                                            "text", "numeric", "numeric", "date", 
                                                                            "text", "numeric", "text", "text", 
                                                                            "text", "text", "date", "date", "text", 
                                                                            "text", "text", "numeric", "numeric", 
                                                                            "text", "text", "text", "text", "text", 
                                                                            "text", "text"))
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

#Cria coluna para join
ssp <- ssp |> mutate(var_join = paste(data_fato, hora_fato, municipio_limpo, sep ="/" )) 

base_limpa <-base_limpa|>  mutate(var_join = paste(data, hora, municipio_limpo, sep ="/" ))

#junta bases - aqui não teve nenhuma conexão entre os casos, repensar as colunas pro join

mdip_unificada <- merge(ssp, base_limpa, by= "var_join", all=TRUE)

# Retira as linhas duplicadas

mdip <- mdip_unificada |> 
  distinct(data_nascimento_pessoa, var_join, .keep_all = TRUE)
