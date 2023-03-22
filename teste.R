library(tidyverse)
base_ssp <- readRDS("./data-raw/mdip_ssp.rds")

base_ssp <- base_ssp |>
  mutate(idade_pessoa_agregada = case_when(
    idade_pessoa > 0 & idade_pessoa < 11 ~ "0-10",
    idade_pessoa >= 11 & idade_pessoa <= 20 ~ "11-20",
    idade_pessoa >= 21 & idade_pessoa <= 30 ~ "21-30",
    idade_pessoa >= 31 & idade_pessoa <= 40 ~ "31-40",
    idade_pessoa >= 41 & idade_pessoa <= 50 ~ "41-50",
    idade_pessoa >= 51 & idade_pessoa <= 60 ~ "51-60",
    idade_pessoa >= 61 ~ "61+")) 

base_ssp <- base_ssp |>                                                  
  mutate(cor_pele_agregada = case_when(
    cor_pele %in% c("Parda","Preta") ~ "Negra",
    cor_pele %in% c("Outros", "NULL","Amarela","Ignorada","REGISTRADO NA PF","Policia Federal") ~ NA,
    TRUE ~ as.character (cor_pele)
  ))

view(base_ssp)

base_ssp |> 
  #filter(!is.na(cor_pele_agregada)) %>%
  #filter(!is.na(idade_pessoa_agregada)) %>%
  group_by(cor_pele, idade_pessoa) %>%
  summarise(total=n()) %>%
  ggplot(aes(x = idade_pessoa, y = total, fill = cor_pele)) +
  geom_bar(position = "dodge", stat = "bin", binwidth = 2) +
  coord_flip() +
  scale_x_reverse() +
  facet_grid(~cor_pele_agregada) +
  labs(title = "Pirâmide etária agrupada por Raça/cor",
       x = "Idade",
       y = "Raça/cor",
       fill = "Raça/cor") +
  theme_classic()
