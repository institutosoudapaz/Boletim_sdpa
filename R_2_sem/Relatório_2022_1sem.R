#setup

library(tidyverse)
library(ggcharts)
library(kableExtra)
library(ggrepel)
library(gt)
libra

#Roda bases 
base_estupros <- readxl::read_excel("../data-raw-2/SIC 565502311342 2013 a 2018 e Metodologia.xlsx", 
                             sheet = "Base de Dados") 

base_estupro_2 <- readxl::read_excel("data-raw-2/SIC 565502311342 2019 a  2023.xlsx")

#Une bases
base_estupros <- rbind(base_estupros, base_estupro_2) |> janitor::clean_names()

#Cria base unificada em RDS
write_rds(base_estupros,"data-raw-2/base_estupros.rds")
base <- readRDS("data-raw-2/base_estupros.rds")
