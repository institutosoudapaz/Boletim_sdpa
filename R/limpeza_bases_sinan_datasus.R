library(read.dbc)
library(tidyverse)

# Abrir arquivos do Sinan ---------------------------------------------------------------------

viol_2013 <- read.dbc("./data-raw/datasus_estupros/VIOLBR13.dbc")|>
  filter (SG_UF_NOT == 35) |> 
  filter (SEX_ESTUPR == 1)

viol_2014 <- read.dbc("./data-raw/datasus_estupros/VIOLBR14.dbc")|>
  filter (SG_UF_NOT == 35) |> 
  filter (SEX_ESTUPR == 1)

viol_2015 <- read.dbc("./data-raw/datasus_estupros/VIOLBR15.dbc")|>
  filter (SG_UF_NOT == 35) |> 
  filter (SEX_ESTUPR == 1)

viol_2016 <- read.dbc("./data-raw/datasus_estupros/VIOLBR16.dbc")|>
  filter (SG_UF_NOT == 35) |> 
  filter (SEX_ESTUPR == 1)

viol_2017 <- read.dbc("./data-raw/datasus_estupros/VIOLBR17.dbc")|>
  filter (SG_UF_NOT == 35) |> 
  filter (SEX_ESTUPR == 1)  

viol_2018 <- read.dbc("./data-raw/datasus_estupros/VIOLBR18.dbc")|>
  filter (SG_UF_NOT == 35) |> 
  filter (SEX_ESTUPR == 1)  

viol_2019 <- read.dbc("./data-raw/datasus_estupros/VIOLBR19.dbc")|>
  filter (SG_UF_NOT == 35) |> 
  filter (SEX_ESTUPR == 1)  

viol_2020 <- read.dbc("./data-raw/datasus_estupros/VIOLBR20.dbc")|>
  filter (SG_UF_NOT == 35) |> 
  filter (SEX_ESTUPR == 1)  

viol_2021 <- read.dbc("./data-raw/datasus_estupros/VIOLBR21.dbc")|>
  filter (SG_UF_NOT == 35) |> 
  filter (SEX_ESTUPR == 1)  

viol_2021 <- read.dbc("./data-raw/datasus_estupros/VIOLBR21.dbc")|>
  filter (SG_UF_NOT == 35) |> 
  filter (SEX_ESTUPR == 1) 

viol_2022 <- read.dbc("./data-raw/datasus_estupros/VIOLBR22.dbc")|>
  filter (SG_UF_NOT == 35) |> 
  filter (SEX_ESTUPR == 1) 


# Combina as bases ----------------------------------------------------------------------------

estupros_sp_sinan <- bind_rows(viol_2013, viol_2014, viol_2015, viol_2016, viol_2017, viol_2018, 
          viol_2019, viol_2020, viol_2021, viol_2022)

# Modifica e limpa a base ---------------------------------------------------------------------

estupros_sp_sinan <- estupros_sp_sinan |>  
  select (-TP_NOT, -ID_AGRAVO, -SEM_NOT, -ID_UNIDADE, -SEM_PRI, -CS_GESTANT, -ID_PAIS,
          -NDUPLIC, -DT_INVEST, -LES_AUTOP, -PEN_ORAL, -PEN_ANAL, -PEN_VAGINA, -PROC_DST,
          -PROC_HIV, -PROC_HEPB, -PROC_SANG, -PROC_SEMEN, -PROC_VAGIN, -PROC_CONTR,
          -PROC_ABORT, -CONS_ABORT, -CONS_GRAV, -CONS_DST, -CONS_SUIC, -CONS_MENT, 
          -CONS_COMP, -CONS_ESTRE, -CONS_OUTR, -CONS_ESPEC, -LESAO_NAT, -LESAO_ESPE,
          -LESAO_CORP, -REL_TRAB, -REL_CAT, -CIRC_LESAO, -CLASSI_FIN, -EVOLUCAO, -DT_OBITO,  
          -DT_DIGITA, -DT_TRANSUS, -DT_TRANSDM, -DT_TRANSSM, -DT_TRANSRM, -DT_TRANSRS,
          -DT_TRANSSE, -TPUNINOT)

writexl::write_xlsx(estupros_sp_sinan, "estupros_sp_sinan.xlsx")

# Exploração da base --------------------------------------------------------------------------

estupros_sp_sinan |> group_by(CS_RACA) |> count()

