Análise exploratória perfil das vítimas de MDIP e COP

  

# Comparação letalidade batalhões com e sem câmera EM SERVIÇO após 2020
base_ssp  |> filter (ano > 2020 & servico == "SIM")|> group_by(COP, ano) |> count()

# Comparação letalidade batalhões antes e depois das câmeras EM SERVIÇO 
base_ssp  |> filter (COP == "SIM" & servico == "SIM")|> group_by(ano) |> count()

# Comparação perfil racial dos batalhões antes e depois das câmeras EM SERVIÇO 
library(viridis)

base_ssp  |> 
  filter (COP == "SIM" & servico == "SIM")|> 
  filter (cor_pele_agregada %in% c("Branca", "Negra")) |> 
  group_by(ano, cor_pele_agregada ) |> 
  count() |> 
  ggplot( aes(x=ano, y=n, group=cor_pele_agregada, color=cor_pele_agregada)) +
  geom_line() + 
  geom_vline(xintercept=c(2020,2021), linetype='dotted', col = 'red')+
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Perfil racial das vítimas de MDIP nos batalhões com COP") +
  theme_minimal()

# Comparação perfil racial dos batalhões com e sem COP antes e depois das câmeras EM SERVIÇO 

base_ssp  |> 
  filter (servico == "SIM" & coorporacao == "PM")|> 
  filter (cor_pele_agregada %in% c("Branca", "Negra")) |> 
  group_by(ano, cor_pele_agregada, COP ) |> 
  count() |> 
  ggplot( aes(x=ano, y=n, group=cor_pele_agregada, color=cor_pele_agregada)) +
  geom_line() + 
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Perfil racial das vítimas de MDIP nos batalhões com e sem COP") +
  theme_minimal() +
  facet_wrap(~ COP)

# Comparação perfil racial dos batalhões com e sem COP antes e depois das câmeras EM SERVIÇO 

base_ssp  |> 
  filter (servico == "SIM" & coorporacao == "PM")|> 
  filter (cor_pele %in% c("Branca", "Preta", "Ignorada")) |> 
  group_by(ano, cor_pele,COP ) |> 
  count() |> 
  ggplot( aes(x=ano, y=n, group=cor_pele, color=cor_pele)) +
  geom_line() + 
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Perfil racial das vítimas de MDIP nos batalhões com e sem COP") +
  theme_minimal() +
  facet_wrap(~ COP)

# Comparação perfil racial dos batalhões com e sem COP antes e depois das câmeras EM SERVIÇO 

base_ssp  |> 
  filter (servico == "SIM" & coorporacao == "PM")|> 
  filter (cor_pele %in% c("Branca", "Preta", "Ignorada")) |> 
  group_by(ano, cor_pele,COP ) |> 
  count() |> 
  ggplot( aes(x=ano, y=n, group=cor_pele, color=cor_pele)) +
  geom_line() + 
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Perfil racial das vítimas de MDIP nos batalhões com e sem COP") +
  theme_minimal() +
  facet_wrap(~ COP)

# Comparação perfil racial dos batalhões com  COP antes e depois das câmeras EM SERVIÇO  E EM FOLGA

base_ssp  |> 
  mutate(servico = case_when(servico %in% c("Sim", "SIM") ~ "Em Serviço",
                             servico == "NÃO" ~ "Folga",
                             TRUE ~ as.character (servico))) |>
  filter (coorporacao == "PM" & COP == "SIM")|> 
  filter (cor_pele_agregada %in% c("Branca", "Negra")) |> 
  group_by(ano, cor_pele_agregada,COP, servico ) |> 
  count() |> 
  ggplot( aes(x=ano, y=n, group=cor_pele_agregada, color=cor_pele_agregada)) +
  geom_line() + 
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Perfil racial das vítimas de MDIP nos batalhões com COP em serviço e folga") +
  theme_minimal() +
  facet_wrap(~ servico)

# Comparação perfil racial dos batalhões SEM  COP antes e depois das câmeras EM SERVIÇO  E EM FOLGA

base_ssp  |> 
  mutate(servico = case_when(servico %in% c("Sim", "SIM") ~ "Em Serviço",
                             servico == "NÃO" ~ "Folga",
                             TRUE ~ as.character (servico))) |>
  filter(!is.na(servico)) |> 
  filter (coorporacao == "PM" & COP == "NÃO")|> 
  filter (cor_pele_agregada %in% c("Branca", "Negra")) |> 
  group_by(ano, cor_pele_agregada,COP, servico ) |> 
  count() |> 
  ggplot( aes(x=ano, y=n, group=cor_pele_agregada, color=cor_pele_agregada)) +
  geom_line() + 
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Perfil racial das vítimas de MDIP nos batalhões SEM COP em serviço e folga") +
  theme_minimal() +
  facet_wrap(~ servico)

# Faixa etária das vítimas de MDIP em batalhões com e sem COP EM TODO O PERIODO

base_ssp |>
  filter(!is.na(idade_pessoa_agregada)) %>%
  filter(ano >= 2018 & ano <= 2022) |> 
  group_by(COP, idade_pessoa_agregada) %>%
  summarise(cnt= n()) %>%
  ggcharts::pyramid_chart(x = idade_pessoa_agregada, y= cnt, group = COP)


# Faixa etária das vítimas de MDIP em batalhões com e sem COP APÓS AS CAMERAS

base_ssp |>
  filter(!is.na(idade_pessoa_agregada)) %>%
  filter(ano > 2020 & ano <= 2022) |> 
  group_by(COP, idade_pessoa_agregada) %>%
  summarise(cnt= n()) %>%
  ggcharts::pyramid_chart(x = idade_pessoa_agregada, y= cnt, group = COP)

# Faixa etária das vítimas de MDIP em todos os batalhões ANTES E APÓS AS CAMERAS (SEM 2020)

base_ssp |>
  filter (coorporacao == "PM") |> 
  filter(!is.na(idade_pessoa_agregada)) %>%
  mutate(periodo = case_when(ano > 2017 & ano < 2020 ~ "Antes COP",
                             ano > 2020 & ano < 2023 ~ "Após COP")) |> 
  filter(!is.na(periodo)) %>%
  group_by(periodo, idade_pessoa_agregada) %>%
  summarise(cnt= n()) %>%
  ggcharts::pyramid_chart(x = idade_pessoa_agregada, y= cnt, group = periodo)

# Comparação local dos MDIP dos batalhões antes e depois das câmeras EM SERVIÇO 

base_ssp  |> 
  filter (COP == "SIM" & servico == "SIM")|> 
  filter (desc_tipolocal %in% c("Via pública", "Residência")) |> 
  group_by(ano, desc_tipolocal ) |> 
  count() |> 
  ggplot( aes(x=ano, y=n, group=desc_tipolocal, color=desc_tipolocal)) +
  geom_line() + 
  geom_vline(xintercept=c(2020,2021), linetype='dotted', col = 'red')+
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Local das ocorrências de MDIP nos batalhões com COP") +
  theme_minimal()

# Comparação local dos MDIP dos batalhões com e sem COP antes e depois das câmeras EM SERVIÇO 

base_ssp  |> 
  filter (servico == "SIM" & coorporacao == "PM")|> 
  filter (desc_tipolocal %in% c("Via pública", "Residência")) |> 
  group_by(ano, desc_tipolocal, COP) |> 
  count() |> 
  ggplot( aes(x=ano, y=n, group=desc_tipolocal, color=desc_tipolocal)) +
  geom_line() + 
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Local das ocorrências  de MDIP nos batalhões com e sem COP") +
  theme_minimal() +
  facet_wrap(~ COP)


# Tabela de variação do MDIP em Serviço antes (2018 e 2019) e depois das COP (2021 e 2022) por municipio

library(DT)

base_ssp  |> 
  filter (servico == "SIM") |> 
  filter(!is.na(idade_pessoa_agregada)) |> 
  mutate(periodo = case_when(ano > 2017 & ano < 2020 ~ "antes_cop",
                             ano > 2020 & ano < 2023 ~ "depois_cop")) |> 
  filter(!is.na(periodo)) |> 
  group_by(municipio_limpo.x, periodo) |> 
  summarise(cnt= n()) |> 
  pivot_wider(names_from = periodo, values_from = cnt) |> 
  mutate(across(where(anyNA), ~ replace_na(., 0))) |> 
  mutate(variacao = round((depois_cop - antes_cop) / antes_cop * 100, 1)) |> 
  select(municipio_limpo.x, antes_cop, depois_cop, variacao) |> 
  datatable(rownames = FALSE, filter = "top", class = 'cell-border stripe', editable = TRUE, 
            colnames = c("Município", "Antes das COP", "Após as COP", "Variação %"))


# Tabela de variação do MDIP em Serviço antes (2018 e 2019) e depois das COP (2021 e 2022) por batalhão

base_ssp  |> 
  filter (servico == "SIM", coorporacao == "PM") |> 
  filter(!is.na(idade_pessoa_agregada)) |> 
  mutate(periodo = case_when(ano > 2017 & ano < 2020 ~ "antes_cop",
                             ano > 2020 & ano < 2023 ~ "depois_cop")) |> 
  filter(!is.na(periodo)) |> 
  group_by(batalhao, periodo, COP) |> 
  summarise(cnt= n()) |> 
  pivot_wider(names_from = periodo, values_from = cnt) |> 
  mutate(across(where(anyNA), ~ replace_na(., 0))) |> 
  mutate(variacao = round((depois_cop - antes_cop) / antes_cop * 100, 1)) |> 
  select(batalhao, antes_cop, depois_cop, variacao, COP) |> 
  datatable(rownames = FALSE, filter = "top", class = 'cell-border stripe', editable = TRUE, 
            colnames = c("Batalhão", "Antes das COP", "Após as COP", "Variação %", "Tem COP"))



