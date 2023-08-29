library(tidyverse)


# Carrega bases -----------------------------------------------------------

estupros_sp_sinan <- readRDS("./data-raw/datasus_estupros/estupros_sp_sinan.rds")


# Análise básica ----------------------------------------------------------

DataExplorer::introduce(estupros_sp_sinan)
DataExplorer::plot_intro(estupros_sp_sinan)
DataExplorer::plot_missing(estupros_sp_sinan)
DataExplorer::plot_bar(estupros_sp_sinan)

DataExplorer::create_report(estupros_sp_sinan)

glimpse(estupros_sp_sinan)

# Ano notificação
estupros_sp_sinan |> group_by(NU_ANO) |> count()

#raça/cor
estupros_sp_sinan |> group_by(CS_RACA) |> count()

# Preechimento bom. Resposta 9 = ignorado, mais o NA, total de 3750 sem info racial, cerca de 8% do banco.

#Tipo local
estupros_sp_sinan |> group_by(LOCAL_OCOR) |> count()

CS_SEXO
estupros_sp_sinan |> group_by(CS_SEXO) |> count()






