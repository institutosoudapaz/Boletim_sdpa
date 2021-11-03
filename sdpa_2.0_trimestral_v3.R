#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
# Projeto: Sou da Paz Analisa 2.0                               #
# Objetivo: Raspagem de dados da SSP (Estatísticas trimestrais) #
# Autor: Elder G. Sant'Anna                                     #
# Data: 18/12/2018                                              #
# 1ª alteração: consertando parcialmente número de linhas e     #
#               compatibilização de duas variáveis              #
# 2ª alteração: Arrumando número de colunas e linhas da base    #
# 3ª alteração: Adapatei a rotina pq os "..." estavam causando  # 
#               problemas para 2018 na rotina anterior.         #
# Versão: v3                                                    #
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

### 1° Passo: Definindo Endereço de armazenagem dos dados e Período

caminho_pasta <- "N:\\Gestão do Conhecimento\\3. PROJETOS\\Sou da Paz Analisa 2.0\\Dados\\dados_ssp_sp\\trimestral"

setwd(caminho_pasta)

periodo <- 1995:2021
{
periodo_anterior <- periodo[1:(length(periodo) - 1)]
periodo_posterior <- periodo[2:length(periodo)]
atual <- max(periodo)
}
### 2° Passo: Escolher se será exportado os dados de anos anteriores
# Já foram exportados todos os dados dos anos anteriores. A menos que 
# os dados sejam perdidos/apagados, só será necessário atualizar os 
# dados recentes.

extrair_ssp <- "NAO" 

### 3° Passo: Escolher se vamos atualizar apenas o ano recente.
# Esse processo deve durar, aproximadamente, 40 minutos.

atualizar_ano_recente <- "SIM"

### 4° Passo: Carregando pacotes necessários
{
#install.packages("data.table")
library(data.table)

#install.packages("devtools")
library(devtools) # Baixar pacotes do github

#install.packages("readxl")
library(readxl)


#install.packages("XML")
library(XML)

#install.packages("xlsx")
library(xlsx)

#install.packages("plyr")
library(plyr) # SEMPRE CARREGAR ANTES DO PACOTE dplyr

#install.packages("png")
library(png)

#install.packages("tabulizer")
library(tabulizer)

### Tidyverse

#install.packages("tidyverse")
library(tidyverse)

#install.packages("lubridate")
library(lubridate)

#install.packages("purrrlyr")
library(purrrlyr) #

### Pacotes de webscraping

#install.packages("httr")
library(httr) # 

#install.packages("rvest")
library(rvest) # 

#install.packages("plyr")
library(plyr) # SEMPRE CARREGAR ANTES DO PACOTE dplyr

#install.packages("Dplyr")
library(dplyr) #

#install.packages("xlsx")
library(xlsx) # Excel
}

### 5° Passo: Extraindo as Estatísticas trimestrais da SSP-SP
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
if (extrair_ssp == "SIM") {
  
  print(paste("Raspando estatísticas trimestrais de", min(periodo_anterior), "a", max(periodo_anterior), "da SSP", sep = " "))
  
  for(i in periodo_anterior)
  {
    for(j in 1:4)
    {
      if (i <= 2003) {
        URL <- paste("http://www.ssp.sp.gov.br/Estatistica/plantrim/",i,"_0",j,".htm",sep="")
        tryCatch({
          pg <- read_html(URL)
          tab <- html_table(pg, fill=TRUE)[[1]]
          tab <- data.frame(tab$X2, tab$X3, tab$X4, tab$X5)
          for(k in 2:ncol(tab))
          {
            tab[[k]] <- as.numeric( gsub("\\.", "", 
                                         gsub("\\(3)", "", 
                                              gsub("\\-", "0",
                                                   tab[[k]]))))
          } 
          tab <- subset.data.frame(tab,is.na(tab.X5)==FALSE)
          tab$ano <- rep(i, nrow(tab))
          tab$tri <- rep(j, nrow(tab))          
          tab$tab.X2 <- as.character(tab$tab.X2)
          Encoding(tab$tab.X2) <- "UTF-8"
          saveRDS(tab, paste("tri_",i,"_",j,".rds",sep=""))
        }, error=function(error_message) {
          
        })  
      } else if ( (i == 2004) ) {
        URL <- paste("http://www.ssp.sp.gov.br/Estatistica/plantrim/",i,"-0",j,".htm",sep="")
        tryCatch({
          pg <- read_html(URL)
          tab <- html_table(pg, fill=TRUE)[[1]]
          tab <- data.frame(tab$X2, tab$X3, tab$X4, tab$X5)
          for(k in 2:ncol(tab))
          {
            tab[[k]] <- as.numeric( gsub("\\.", "", 
                                         gsub("\\(3)", "", 
                                              gsub("\\-", "0",
                                                   tab[[k]]))))
          } 
          tab <- subset.data.frame(tab,is.na(tab.X3)==FALSE)
          tab$ano <- rep(i, nrow(tab))
          tab$tri <- rep(j, nrow(tab))     
          tab$tab.X2 <- as.character(tab$tab.X2)
          Encoding(tab$tab.X2) <- "UTF-8"
          saveRDS(tab, paste("tri_",i,"_",j,".rds",sep=""))
        }, error=function(error_message) {
          
        }) 
      } else if ( (i > 2004) & (i < 2006) ) {
        URL <- paste("http://www.ssp.sp.gov.br/Estatistica/plantrim/",i,"-0",j,".htm",sep="")
        tryCatch({
          pg <- read_html(URL)
          tab <- html_table(pg, fill=TRUE)[[1]]
          tab <- data.frame(tab$X2, tab$X3, tab$X4,  tab$X5, tab$X6, tab$X7, tab$X8,  
                            tab$X9, tab$X10, tab$X11, tab$X12, tab$X13)
          for(k in 2:ncol(tab))
          {
            tab[[k]] <- as.numeric( gsub("\\.", "", 
                                         gsub("\\(3)", "", 
                                              gsub("\\-", "0",
                                                   tab[[k]]))))
          } 
          tab <- subset.data.frame(tab,is.na(tab.X3)==FALSE)
          tab$ano <- rep(i, nrow(tab))
          tab$tri <- rep(j, nrow(tab)) 
          tab$tab.X2 <- as.character(tab$tab.X2)
          Encoding(tab$tab.X2) <- "UTF-8"
          saveRDS(tab, paste("tri_",i,"_",j,".rds",sep=""))
        }, error=function(error_message) {
          
        })   
      } else if ( (i == 2006) & (j < 3) ) {
        URL <- paste("http://www.ssp.sp.gov.br/Estatistica/plantrim/",i,"-0",j,".htm",sep="")
        tryCatch({
          pg <- read_html(URL)
          tab <- html_table(pg, fill=TRUE)[[1]]
          tab <- data.frame(tab$X2, tab$X3, tab$X4,  tab$X5, tab$X6, tab$X7, tab$X8,  
                            tab$X9, tab$X10, tab$X11, tab$X12, tab$X13)
          for(k in 2:ncol(tab))
          {
            tab[[k]] <- as.numeric( gsub("\\.", "", 
                                         gsub("\\(3)", "", 
                                              gsub("\\-", "0",
                                                   tab[[k]]))))
          } 
          tab <- subset.data.frame(tab,is.na(tab.X3)==FALSE)
          tab$ano <- rep(i, nrow(tab))
          tab$tri <- rep(j, nrow(tab)) 
          tab$tab.X2 <- as.character(tab$tab.X2)
          Encoding(tab$tab.X2) <- "UTF-8"
          saveRDS(tab, paste("tri_",i,"_",j,".rds",sep=""))
        }, error=function(error_message) {
          
        })   
      } else if ( (i == 2006) & (j > 2) ) {
        URL <- paste("http://www.ssp.sp.gov.br/Estatistica/plantrim/",i,"-0",j,".htm",sep="")
        tryCatch({
          pg <- read_html(URL)
          tab <- html_table(pg, fill=TRUE)[[1]]
          tab <- data.frame(tab$X2, tab$X3, tab$X4,  tab$X5, tab$X6, tab$X7, tab$X8,  
                            tab$X9, tab$X10, tab$X11, tab$X12, tab$X13, tab$X14)
          for(k in 2:ncol(tab))
          {
            tab[[k]] <- as.numeric( gsub("\\.", "", 
                                         gsub("\\(3)", "", 
                                              gsub("\\-", "0",
                                                   tab[[k]]))))
          } 
          tab <- subset.data.frame(tab,is.na(tab.X3)==FALSE)
          tab$ano <- rep(i, nrow(tab))
          tab$tri <- rep(j, nrow(tab)) 
          tab$tab.X2 <- as.character(tab$tab.X2)
          Encoding(tab$tab.X2) <- "UTF-8"
          saveRDS(tab, paste("tri_",i,"_",j,".rds",sep=""))
        }, error=function(error_message) {
          
        })   
      } else if ( (i > 2006) & (i < 2013) ) {
        URL <- paste("http://www.ssp.sp.gov.br/Estatistica/plantrim/",i,"-0",j,".htm",sep="")
        tryCatch({
          pg <- read_html(URL)
          tab <- html_table(pg, fill=TRUE)[[1]]
          tab <- data.frame(tab$X2, tab$X3, tab$X4,  tab$X5, tab$X6, tab$X7, tab$X8,  
                            tab$X9, tab$X10, tab$X11, tab$X12, tab$X13, tab$X14) 
          for(k in 2:ncol(tab))
          {
            tab[[k]] <- as.numeric( gsub("\\.", "", 
                                         gsub("\\(3)", "", 
                                              gsub("\\-", "0",
                                                   tab[[k]]))))
          } 
          tab <- subset.data.frame(tab,is.na(tab.X3)==FALSE)
          tab$ano <- rep(i, nrow(tab))
          tab$tri <- rep(j, nrow(tab))
          tab$tab.X2 <- as.character(tab$tab.X2)
          Encoding(tab$tab.X2) <- "UTF-8"
          saveRDS(tab, paste("tri_",i,"_",j,".rds",sep=""))
        }, error=function(error_message) {
          
        })   
      } else if ( (i == 2013) & (j < 3) ) {
        URL <- paste("http://www.ssp.sp.gov.br/Estatistica/plantrim/",i,"-0",j,".htm",sep="")
        tryCatch({
          pg <- read_html(URL)
          tab <- html_table(pg, fill=TRUE)[[1]]
          tab <- data.frame(tab$X2, tab$X3, tab$X4,  tab$X5, tab$X6, tab$X7, tab$X8,  
                            tab$X9, tab$X10, tab$X11, tab$X12, tab$X13, tab$X14) 
          for(k in 2:ncol(tab))
          {
            tab[[k]] <- as.numeric( gsub("\\.", "", 
                                         gsub("\\(3)", "", 
                                              gsub("\\-", "0",
                                                   tab[[k]]))))
          } 
          tab <- subset.data.frame(tab,is.na(tab.X3)==FALSE)
          tab$ano <- rep(i, nrow(tab))
          tab$tri <- rep(j, nrow(tab))
          tab$tab.X2 <- as.character(tab$tab.X2)
          Encoding(tab$tab.X2) <- "UTF-8"
          saveRDS(tab, paste("tri_",i,"_",j,".rds",sep=""))
        }, error=function(error_message) {
          
        })   
      }  else if ( (i == 2013) & (j > 2) ) {
        URL <- paste("http://www.ssp.sp.gov.br/Estatistica/plantrim/",i,"-0",j,".htm",sep="")
        tryCatch({
          pg <- read_html(URL)
          tab <- html_table(pg, fill=TRUE)[[1]]
          tab <- data.frame(tab$X2, tab$X3, tab$X4,  tab$X5, tab$X6, tab$X7, tab$X8,  
                            tab$X9, tab$X10, tab$X11, tab$X12, tab$X13, tab$X14, tab$X15) 
          for(k in 2:ncol(tab))
          {
            tab[[k]] <- as.numeric( gsub("\\.", "", 
                                         gsub("\\(3)", "", 
                                              gsub("\\-", "0",
                                                   tab[[k]]))))
          } 
          tab <- subset.data.frame(tab,is.na(tab.X3)==FALSE)
          tab$ano <- rep(i, nrow(tab))
          tab$tri <- rep(j, nrow(tab))
          tab$tab.X2 <- as.character(tab$tab.X2)
          Encoding(tab$tab.X2) <- "UTF-8"
          saveRDS(tab, paste("tri_",i,"_",j,".rds",sep=""))
        }, error=function(error_message) {
          
        })   
      } else if (i > 2013) {
        URL <- paste("http://www.ssp.sp.gov.br/Estatistica/plantrim/",i,"-0",j,".htm",sep="")
        tryCatch({
          pg <- read_html(URL)
          tab <- html_table(pg, fill=TRUE)[[1]]
          tab <- data.frame(tab$X2, tab$X3, tab$X4,  tab$X5, tab$X6, tab$X7, tab$X8,  
                       tab$X9, tab$X10, tab$X11, tab$X12, tab$X13, tab$X14, tab$X15) 
          for(k in 1:ncol(tab))  {
            for (j in 1:nrow(tab) ) {
              if (tab[[j,k]]== "..."){
                tab[[j,k]] <- "0"
              } else {
                
              }
              
            }
          }
          tab <- data.frame(tab$X2, tab$X3, tab$X4,  tab$X5, tab$X6, tab$X7, tab$X8,  
                            tab$X9, tab$X10, tab$X11, tab$X12, tab$X13, tab$X14, tab$X15)
          for(k in 2:ncol(tab))
          {
            tab[[k]] <- as.numeric(gsub("\\.", "", 
                                        gsub("\\(3)", "", 
                                             gsub("\\-", "0",
                                                  tab[[k]]))))
          } 
          tab <- subset.data.frame(tab,is.na(tab.X3)==FALSE)
          tab$ano <- rep(i, nrow(tab))
          tab$tri <- rep(j, nrow(tab))   
          tab$tab.X2 <- as.character(tab$tab.X2)
          Encoding(tab$tab.X2) <- "UTF-8"
          saveRDS(tab, paste("tri_",i,"_",j,".rds",sep=""))
        }, error=function(error_message) {
          
        })       
      }
    }
  }
  
  # Para o 1°, 3° e 4° trimestre de 2001 a página é diferente 
  # e, portanto, requer tratamento diferenciado
  for (j in c(1,3,4)) {
    URL <- paste("http://www.ssp.sp.gov.br/Estatistica/plantrim/2001_0",j,".htm",sep="")
    teste <- readHTMLTable(URL)[[1]]
    for (i in 2:6 )  
    {
      teste[[i]] <- gsub("\\.", "", teste[[i]])
    }
    
    for (i in 1:nrow(teste) ) {
      if ( (is.na(teste[i,6])==TRUE)&(is.na(teste[i,5])==FALSE) ){
        teste[i,6] <- teste[i,5]
        teste[i,5] <- teste[i,4]
        teste[i,4] <- teste[i,3]
        teste[i,3] <- teste[i,2]
        teste[i,2] <- as.character(teste[i,1])
      }
    }
    
    for (i in 3:6 ) {
      teste[[i]] <- as.numeric(teste[[i]])
    }
    
    tab <- subset.data.frame(teste, is.na(teste[6])==FALSE)
    names(tab) <- c("X1", "X2", "X3", "X4", "X5", "X6")
    tab <- data.frame(tab$X2, tab$X3, tab$X4, tab$X5)
    for(k in 2:ncol(tab))
    {
      tab[[k]] <- as.numeric( gsub("\\.", "", 
                                   gsub("\\(3)", "", 
                                        gsub("\\-", "0",
                                             tab[[k]]))))
    } 
    #tab <- subset.data.frame(tab,is.na(tab.X3)==FALSE)
    tab$ano <- rep(2001, nrow(tab))
    tab$tri <- rep(j, nrow(tab))
    tab$tab.X2 <- as.character(tab$tab.X2)
    Encoding(tab$tab.X2) <- "UTF-8"
    saveRDS(tab, paste("tri_2001_",j,".rds",sep=""))  
  }
  
  # Para o 2° trimestre de 2002 a página é diferente 
  # e, portanto, requer tratamento diferenciado
  
  URL <- "http://www.ssp.sp.gov.br/Estatistica/plantrim/2002_02.htm"
  pg <- read_html(URL)
  tab <- html_table(pg, fill=TRUE)[[1]]
  for (k in 2:6) {
    tab2 <- html_table(pg, fill=TRUE)[[k]]  
    tab <- rbind(tab, tab2)
  }
  tab <- cbind.data.frame(tab$X2, tab$X3, tab$X4, tab$X5)
  for(k in 2:ncol(tab))
  {
    tab[[k]] <- as.numeric( gsub("\\.", "", 
                                 gsub("\\(3)", "", 
                                      gsub("\\-", "0",
                                           tab[[k]]))))
  } 
  colnames(tab) <- c("tab.X2", "tab.X3", "tab.X4", "tab.X5")
  tab <- subset.data.frame(tab,is.na(tab.X3)==FALSE)
  tab$ano <- rep(2002, nrow(tab))
  tab$tri <- rep(2, nrow(tab))
  tab$tab.X2 <- as.character(tab$tab.X2)
  Encoding(tab$tab.X2) <- "UTF-8"
  saveRDS(tab, paste("tri_2002_2.rds",sep=""))
  
} else {
  
  warning(paste("Não serão raspados dados antigos da SSP"))
  
  warning(paste("Se você deseja raspar dados de", min(periodo_anterior), "a", max(periodo_anterior), "da SSP, insira 'SIM' na variavel extrair_ssp"))
  
  Sys.sleep(10)
  
}
  
if (atualizar_ano_recente == "SIM") {
  
  print(paste("Raspando estatísticas trimestrais", max(periodo), "da SSP", sep = " "))
  
  for(i in max(periodo))
  {
    for(j in 1:4)
    {
      if (i <= 2003) {
        URL <- paste("http://www.ssp.sp.gov.br/Estatistica/plantrim/",i,"_0",j,".htm",sep="")
        tryCatch({
          pg <- read_html(URL)
          tab <- html_table(pg, fill=TRUE)[[1]]
          tab <- cbind(tab$X2, tab$X3, tab$X4, tab$X5, tab$X6)
          for(k in 2:ncol(tab))
          {
            tab[[k]] <- as.numeric( gsub("\\.", "", 
                                         gsub("\\(3)", "", 
                                              gsub("\\-", "0",
                                                   tab[[k]]))))
          } 
          tab <- subset.data.frame(tab,is.na(tab.X6)==FALSE)
          tab$ano <- rep(i, nrow(tab))
          tab$tri <- rep(j, nrow(tab))
          tab$tab.X2 <- as.character(tab$tab.X2)
          Encoding(tab$tab.X2) <- "UTF-8"
          saveRDS(tab, paste("tri_",i,"_",j,".rds",sep=""))
        }, error=function(error_message) {
          
        })  
      }  else if ( (i == 2004) ) {
        URL <- paste("http://www.ssp.sp.gov.br/Estatistica/plantrim/",i,"-0",j,".htm",sep="")
        tryCatch({
          pg <- read_html(URL)
          tab <- html_table(pg, fill=TRUE)[[1]]
          tab <- data.frame(tab$X2, tab$X3, tab$X4,  tab$X5)  
          for(k in 2:ncol(tab))
          {
            tab[[k]] <- as.numeric( gsub("\\.", "", 
                                         gsub("\\(3)", "", 
                                              gsub("\\-", "0",
                                                   tab[[k]]))))
          } 
          tab <- subset.data.frame(tab,is.na(tab.X3)==FALSE)
          tab$ano <- rep(i, nrow(tab))
          tab$tri <- rep(j, nrow(tab))  
          tab$tab.X2 <- as.character(tab$tab.X2)
          Encoding(tab$tab.X2) <- "UTF-8"
          saveRDS(tab, paste("tri_",i,"_",j,".rds",sep=""))
        }, error=function(error_message) {
          
        }) 
      } else if ( (i > 2004) & (i < 2006) ) {
        URL <- paste("http://www.ssp.sp.gov.br/Estatistica/plantrim/",i,"-0",j,".htm",sep="")
        tryCatch({
          pg <- read_html(URL)
          tab <- html_table(pg, fill=TRUE)[[1]]
          tab <- data.frame(tab$X2, tab$X3, tab$X4,  tab$X5, tab$X6, tab$X7, tab$X8,  
                            tab$X9, tab$X10, tab$X11, tab$X12, tab$X13)
          for(k in 2:ncol(tab))
          {
            tab[[k]] <- as.numeric( gsub("\\.", "", 
                                         gsub("\\(3)", "", 
                                              gsub("\\-", "0",
                                                   tab[[k]]))))
          } 
          tab <- subset.data.frame(tab,is.na(tab.X3)==FALSE)
          tab$ano <- rep(i, nrow(tab))
          tab$tri <- rep(j, nrow(tab)) 
          tab$tab.X2 <- as.character(tab$tab.X2)
          Encoding(tab$tab.X2) <- "UTF-8"
          saveRDS(tab, paste("tri_",i,"_",j,".rds",sep=""))
        }, error=function(error_message) {
          
        })   
      } else if ( (i == 2006) & (j < 3) ) {
        URL <- paste("http://www.ssp.sp.gov.br/Estatistica/plantrim/",i,"-0",j,".htm",sep="")
        tryCatch({
          pg <- read_html(URL)
          tab <- html_table(pg, fill=TRUE)[[1]]
          tab <- data.frame(tab$X2, tab$X3, tab$X4,  tab$X5, tab$X6, tab$X7, tab$X8,  
                            tab$X9, tab$X10, tab$X11, tab$X12, tab$X13)
          for(k in 2:ncol(tab))
          {
            tab[[k]] <- as.numeric( gsub("\\.", "", 
                                         gsub("\\(3)", "", 
                                              gsub("\\-", "0",
                                                   tab[[k]]))))
          } 
          tab <- subset.data.frame(tab,is.na(tab.X3)==FALSE)
          tab$ano <- rep(i, nrow(tab))
          tab$tri <- rep(j, nrow(tab)) 
          tab$tab.X2 <- as.character(tab$tab.X2)
          Encoding(tab$tab.X2) <- "UTF-8"
          saveRDS(tab, paste("tri_",i,"_",j,".rds",sep=""))
        }, error=function(error_message) {
          
        })   
      } else if ( (i == 2006) & (j > 2) ) {
        URL <- paste("http://www.ssp.sp.gov.br/Estatistica/plantrim/",i,"-0",j,".htm",sep="")
        tryCatch({
          pg <- read_html(URL)
          tab <- html_table(pg, fill=TRUE)[[1]]
          tab <- data.frame(tab$X2, tab$X3, tab$X4,  tab$X5, tab$X6, tab$X7, tab$X8,  
                            tab$X9, tab$X10, tab$X11, tab$X12, tab$X13, tab$X14)
          for(k in 2:ncol(tab))
          {
            tab[[k]] <- as.numeric( gsub("\\.", "", 
                                         gsub("\\(3)", "", 
                                              gsub("\\-", "0",
                                                   tab[[k]]))))
          } 
          tab <- subset.data.frame(tab,is.na(tab.X3)==FALSE)
          tab$ano <- rep(i, nrow(tab))
          tab$tri <- rep(j, nrow(tab)) 
          tab$tab.X2 <- as.character(tab$tab.X2)
          Encoding(tab$tab.X2) <- "UTF-8"
          saveRDS(tab, paste("tri_",i,"_",j,".rds",sep=""))
        }, error=function(error_message) {
          
        })   
      } else if ( (i > 2006) & (i < 2013) ) {
        URL <- paste("http://www.ssp.sp.gov.br/Estatistica/plantrim/",i,"-0",j,".htm",sep="")
        tryCatch({
          pg <- read_html(URL)
          tab <- html_table(pg, fill=TRUE)[[1]]
          tab <- data.frame(tab$X2, tab$X3, tab$X4,  tab$X5, tab$X6, tab$X7, tab$X8,  
                            tab$X9, tab$X10, tab$X11, tab$X12, tab$X13, tab$X14) 
          for(k in 2:ncol(tab))
          {
            tab[[k]] <- as.numeric( gsub("\\.", "", 
                                         gsub("\\(3)", "", 
                                              gsub("\\-", "0",
                                                   tab[[k]]))))
          } 
          tab <- subset.data.frame(tab,is.na(tab.X3)==FALSE)
          tab$ano <- rep(i, nrow(tab))
          tab$tri <- rep(j, nrow(tab))
          tab$tab.X2 <- as.character(tab$tab.X2)
          Encoding(tab$tab.X2) <- "UTF-8"
          saveRDS(tab, paste("tri_",i,"_",j,".rds",sep=""))
        }, error=function(error_message) {
          
        })   
      } else if ( (i == 2013) & (j < 3) ) {
        URL <- paste("http://www.ssp.sp.gov.br/Estatistica/plantrim/",i,"-0",j,".htm",sep="")
        tryCatch({
          pg <- read_html(URL)
          tab <- html_table(pg, fill=TRUE)[[1]]
          tab <- data.frame(tab$X2, tab$X3, tab$X4,  tab$X5, tab$X6, tab$X7, tab$X8,  
                            tab$X9, tab$X10, tab$X11, tab$X12, tab$X13, tab$X14) 
          for(k in 2:ncol(tab))
          {
            tab[[k]] <- as.numeric( gsub("\\.", "", 
                                         gsub("\\(3)", "", 
                                              gsub("\\-", "0",
                                                   tab[[k]]))))
          } 
          tab <- subset.data.frame(tab,is.na(tab.X3)==FALSE)
          tab$ano <- rep(i, nrow(tab))
          tab$tri <- rep(j, nrow(tab))
          tab$tab.X2 <- as.character(tab$tab.X2)
          Encoding(tab$tab.X2) <- "UTF-8"
          saveRDS(tab, paste("tri_",i,"_",j,".rds",sep=""))
        }, error=function(error_message) {
          
        })   
      }  else if ( (i == 2013) & (j > 2) ) {
        URL <- paste("http://www.ssp.sp.gov.br/Estatistica/plantrim/",i,"-0",j,".htm",sep="")
        tryCatch({
          pg <- read_html(URL)
          tab <- html_table(pg, fill=TRUE)[[1]]
          tab <- data.frame(tab$X2, tab$X3, tab$X4,  tab$X5, tab$X6, tab$X7, tab$X8,  
                            tab$X9, tab$X10, tab$X11, tab$X12, tab$X13, tab$X14, tab$X15) 
          for(k in 2:ncol(tab))
          {
            tab[[k]] <- as.numeric( gsub("\\.", "", 
                                         gsub("\\(3)", "", 
                                              gsub("\\-", "0",
                                                   tab[[k]]))))
          } 
          tab <- subset.data.frame(tab,is.na(tab.X3)==FALSE)
          tab$ano <- rep(i, nrow(tab))
          tab$tri <- rep(j, nrow(tab))
          tab$tab.X2 <- as.character(tab$tab.X2)
          Encoding(tab$tab.X2) <- "UTF-8"
          saveRDS(tab, paste("tri_",i,"_",j,".rds",sep=""))
        }, error=function(error_message) {
          
        })   
      } else if (i > 2013) {
        URL <- paste("http://www.ssp.sp.gov.br/Estatistica/plantrim/",i,"-0",j,".htm",sep="")
        tryCatch({
          pg <- read_html(URL)
          tab <- html_table(pg, fill=TRUE)[[1]]
          for(k in 1:ncol(tab))  {
            for (l in 1:nrow(tab) ) {
              if (tab[[l,k]]== "..."){
                tab[[l,k]] <- "0"
              } else {
                
              }
              
            }
          }
          tab <- data.frame(tab$X2, tab$X3, tab$X4,  tab$X5, tab$X6, tab$X7, tab$X8,  
                            tab$X9, tab$X10, tab$X11, tab$X12, tab$X13, tab$X14, tab$X15)
          for(k in 2:ncol(tab))
          {
            tab[[k]] <- as.numeric(gsub("\\.", "", 
                                       gsub("\\(3)", "", 
                                            gsub("\\-", "0",
                                                 tab[[k]]))))
          } 
          tab <- subset.data.frame(tab,is.na(tab.X3)==FALSE)
          tab$ano <- rep(i, nrow(tab))
          tab$tri <- rep(j, nrow(tab))
          tab$tab.X2 <- as.character(tab$tab.X2)
          Encoding(tab$tab.X2) <- "UTF-8"
          saveRDS(tab, paste("tri_",i,"_",j,".rds",sep=""))
        }, error=function(error_message) {
          
        })       
      }
    }
  }   
    
} else {
  
  warning(paste("Não serão raspados dados novos de", max(periodo), "da SSP", sep = " "))
  
  warning(paste("Se você deseja raspar dados de", max(periodo), "da SSP, insira 'SIM' na variavel atualizar_ano_recente"))
  
  Sys.sleep(10)
  
}  
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

### 6° Passo: Agregando todos os dados trimestrais em uma única base

base_tri <- readRDS("tri_1995_3.rds")
tabela <- readRDS("tri_1995_4.rds")
base_tri <- full_join(base_tri, tabela)

for(i in periodo_posterior)
{
 for(j in 1:4)
  {
     tabela <- readRDS(paste("tri_",i,"_",j,".rds",sep=""))
     base_tri <- full_join(base_tri, tabela)
  }
}

colnames(base_tri) <- c("natureza", "kap", "gsp", "int", "ano", "tri", "dei1", "dei2",
                       "dei3", "dei4", "dei5", "dei6", "dei7", "dei8", "dei9", "de10")

# 7° Passo: Corrigindo problemas de codificação no nome das variáveis
# para poder compatibilizar as variáveis ao longo dos anos
base_tri$natureza <- toupper(base_tri$natureza)
base_tri$natureza <- str_trim(base_tri$natureza)
base_tri[[1]] <- gsub("\\\r\n ", "", base_tri[[1]])
base_tri[[1]] <- gsub("\\PATRIMONIO", "PATRIMÔNIO", base_tri[[1]])
base_tri[[1]] <- gsub("\\VEÍCULOS", "VEÍCULO", base_tri[[1]])
base_tri[[1]] <- gsub("\\ROUBOS", "ROUBO", base_tri[[1]])
base_tri[[1]] <- gsub("\\FURTOS", "FURTO", base_tri[[1]])
base_tri[[1]] <- gsub("\\CONF. POL.                   CIV.", "CONF POL CIV", base_tri[[1]])
base_tri[[1]] <- gsub("\\CONF. POL.               MIL.", "CONF POL MIL", base_tri[[1]])
base_tri[[1]] <- gsub("\\TOT DE BOL", "TOT. DE BOL.", base_tri[[1]])
base_tri[[1]] <- gsub("\\TOT. DE BOL.", "TOTAL DE BOLETINS", base_tri[[1]])
base_tri[[1]] <- gsub("\\TOTAL DE TERMOS CIRCUNSTANCIADOS LAVRADOS", "TOT. DE TERMOS CIRC. LAV.", base_tri[[1]])
base_tri[[1]] <- gsub("\\PELA POL. CIV.", "PELA POLÍCIA CIVIL", base_tri[[1]])
base_tri[[1]] <- gsub("\\PELA POL. MIL.", "PELA POLÍCIA MILITAR", base_tri[[1]])
base_tri[[1]] <- gsub("\\POL MIL", "POL. MIL.", base_tri[[1]])
base_tri[[1]] <- gsub("\\POL CIV", "POL. CIV.", base_tri[[1]])
base_tri[[1]] <- gsub("\\POLICIAIS MILITARES", "POL. MIL.", base_tri[[1]])
base_tri[[1]] <- gsub("\\POLICIAIS CIVIS", "POL. CIV.", base_tri[[1]])
base_tri[[1]] <- gsub("\\EM CONFRONTO COM A POLÍCIA CIVIL", "EM CONF.POL.CIV.", base_tri[[1]])
base_tri[[1]] <- gsub("\\EM CONFRONTO COM A POLÍCIA MILITAR", "EM CONF.POL.MIL.", base_tri[[1]])
base_tri[[1]] <- gsub("\\EM CONF. POL. CIV.", "EM CONF.POL.CIV.", base_tri[[1]])
base_tri[[1]] <- gsub("\\EM CONF. POL. MIL.", "EM CONF.POL.MIL.", base_tri[[1]])
base_tri[[1]] <- gsub("\\EM CONF POL. CIV.", "EM CONF.POL.CIV.", base_tri[[1]])
base_tri[[1]] <- gsub("\\EM CONF POL. MIL.", "EM CONF.POL.MIL.", base_tri[[1]])
base_tri[[1]] <- gsub("\\ART. 173", "ART 173", base_tri[[1]])
base_tri[[1]] <- gsub("\\DELITOS CONTRA", "CONTRA", base_tri[[1]])
base_tri[[1]] <- gsub("\\DELATROCÍNIO", "DE LATROCÍNIO", base_tri[[1]])

# 8° Passo: Compatibilizando a variável estupro
# Para maiores detlahes ver notas metodológicas

base_tri[[1]] <- gsub("\\ESTUPRO TOTAL", "ESTUPRO", base_tri[[1]])
base_tri$natureza[(base_tri$ano >= 2017)&(base_tri$natureza == "ESTUPRO") ] <- "ESTUPRO NÃO VULNERÁVEL"

# 9° Passo: Compatibilizando a variável roubo
# Para maiores detlahes ver notas metodológicas
base_tri$natureza[(base_tri$ano >= 2017)&(base_tri$natureza == "ROUBO - OUTROS") ] <- "ROUBO - OUTRO"

# 10° Passo: Codificando as variáveis
{
natureza <- c('ARMAS DE FOGO APREENDIDAS',
              'CONTRA A DIGNIDADE SEXUAL',
              'CONTRA A PESSOA',
              'CONTRA COSTUMES',
              'CONTRA O PATRIMÔNIO',
              'CONTRA OS COSTUMES',
              'CONTRA PATRIMÔNIO',
              'CONTRA PESSOA',
              'CONTRAVENCIONAIS',
              'ENTORPECENTES',
              'ESTUPRO',
              'ESTUPRO (17)',
              'ESTUPRO DE VULNERÁVEL',
              'ESTUPRO NÃO VULNERÁVEL',
              'EXAMES CLÍNICOS LABORATORIAIS REALIZADOS - IML (14)',
              'EXAMES CLÍNICOS LABORATORIAIS REALIZADOS - IML(4)',
              'EXAMES CLÍNICOS LABORATORIAIS REALIZADOS - IML(5)',
              'EXAMES NECROSCÓPICOS REALIZADOS - IML',
              'EXAMES NECROSCÓPICOS REALIZADOS - IML(4)',
              'EXAMES NECROSCÓPICOS REALIZADOS - IML(5)',
              'EXAMES OUTROS REALIZADOS - IML (14)',
              'EXAMES PERICIAIS REALIZADOS - IC',
              'EXAMES PERICIAIS REALIZADOS - IC(4)',
              'EXAMES PERICIAIS REALIZADOS - IC(5)',
              'EXTORSÃO MEDIANTE SEQÜESTRO (2)',
              'EXTORSÃO MEDIANTE SEQÜESTRO (4)',
              'EXTORSÃO MEDIANTE SEQÜESTRO (5)',
              'EXTORSÃO MEDIANTE SEQÜESTRO (6)',
              'FURTO',
              'FURTO - OUTROS',
              'FURTO DE VEÍCULO',
              'HOMICÍDIO CULPOSO',
              'HOMICÍDIO CULPOSO (7)',
              'HOMICÍDIO CULPOSO (8)',
              'HOMICÍDIO CULPOSO POR ACIDENTE DE TRÂNSITO',
              'HOMICÍDIO CULPOSO(7)',
              'HOMICÍDIO CULPOSO(8)',
              'HOMICÍDIO DOLOSO',
              'HOMICÍDIO DOLOSO POR ACIDENTE DE TRÂNSITO (10)',
              'HOMICÍDIO DOLOSO POR ACIDENTE DE TRÂNSITO (11)',
              'LATROCÍNIO',
              'LAUDOS CLÍNICOS LABORATORIAIS EXPEDIDOS - IML(4)',
              'LAUDOS CLÍNICOS LABORATORIAIS EXPEDIDOS - IML(5)',
              'LAUDOS CLÍNICOS LABORATORIAIS EXPEDIDOS DO ANO - IML (15)',
              'LAUDOS CLÍNICOS LABORATORIAIS EXPEDIDOS DO ANO ANTERIOR - IML (16)',
              'LAUDOS CLÍNICOS LABORATORIAIS EXPEDIDOS DO ANO ANTERIOR - IML(5)',
              'LAUDOS NECROSCÓPICOS EXPEDIDOS - IML(4)',
              'LAUDOS NECROSCÓPICOS EXPEDIDOS - IML(5)',
              'LAUDOS NECROSCÓPICOS EXPEDIDOS DO ANO - IML',
              'LAUDOS NECROSCÓPICOS EXPEDIDOS DO ANO ANTERIOR IML',
              'LAUDOS NECROSCÓPICOS EXPEDIDOS DO ANO ANTERIOR IML(5)',
              'LAUDOS OUTROS EXPEDIDOS DO ANO - IML (15)',
              'LAUDOS OUTROS EXPEDIDOS DO ANO ANTERIOR IML (16)',
              'LAUDOS PERICIAIS EXPEDIDOS - IC(4)',
              'LAUDOS PERICIAIS EXPEDIDOS - IC(5)',
              'LAUDOS PERICIAIS EXPEDIDOS DO ANO - IC',
              'LAUDOS PERICIAIS EXPEDIDOS DO ANO ANTERIOR- IC',
              'LAUDOS PERICIAIS EXPEDIDOS DO ANO ANTERIOR- IC(5)',
              'LESÃO CORPORAL (CULP. E DOL.)',
              'LESÃO CORPORAL CULPOSA',
              'LESÃO CORPORAL CULPOSA OUTRAS',
              'LESÃO CORPORAL CULPOSA POR ACIDENTE DE TRÂNSITO',
              'LESÃO CORPORAL DOLOSA',
              'LESÃO CORPORAL SEGUIDA DE MORTE',
              'NÃO CRIMINAIS',
              'Nº DE AUTOS DE APREENSÃO (ART 173 ECA)',
              'Nº DE INFRATORES APREENDIDOS EM FLAGRANTE',
              'Nº DE INFRATORES APREENDIDOS POR MANDADO',
              'Nº DE PESSOAS PRESAS EM FLAGRANTE',
              'Nº DE PESSOAS PRESAS POR MANDADO',
              'Nº DE REVISTAS PESSOAIS/ IDENTIFICAÇÃO',
              'Nº DE REVISTAS PESSOAIS/ IDENTIFICAÇÃO (10)',
              'Nº DE REVISTAS PESSOAIS/ IDENTIFICAÇÃO (9)',
              'Nº DE VEÍCULO RECUPERADOS',
              'Nº DE VÍTIMAS DE LATROCÍNIO',
              'Nº DE VÍTIMAS EM HOMICÍDIO DOLOSO',
              'Nº DE VÍTIMAS EM HOMICÍDIO DOLOSO POR ACIDENTE DE TRÂNSITO (11)',
              'Nº DE VÍTIMAS EM HOMICÍDIO DOLOSO POR ACIDENTE DE TRÂNSITO (12)',
              'OUTROS CRIMINAIS (INCLUI CONTRAVENÇÕES)',
              'OUTROS CRIMINAIS (NÃO INCLUI CONTRAVENÇÕES)',
              'OUTROS DELITOS (INCLUI CONTRAVENÇÕES)',
              'PESSOAS FERIDAS EM CONF.POL.CIV.',
              'PESSOAS FERIDAS EM CONF.POL.CIV. EM SERVIÇO',
              'PESSOAS FERIDAS EM CONF.POL.MIL.',
              'PESSOAS FERIDAS EM CONF.POL.MIL. EM SERVIÇO',
              'PESSOAS FERIDAS POR POL. CIV. DE FOLGA',
              'PESSOAS FERIDAS POR POL. MIL. DE FOLGA',
              'PESSOAS MORTAS EM CONF.POL.CIV.',
              'PESSOAS MORTAS EM CONF.POL.CIV. EM SERVIÇO',
              'PESSOAS MORTAS EM CONF.POL.CIV. EM SERVIÇO (7)',
              'PESSOAS MORTAS EM CONF.POL.CIV. EM SERVIÇO (8)',
              'PESSOAS MORTAS EM CONF.POL.CIV. EM SERVIÇO (9)',
              'PESSOAS MORTAS EM CONF.POL.CIV. EM SERVIÇO(9)',
              'PESSOAS MORTAS EM CONF.POL.MIL.',
              'PESSOAS MORTAS EM CONF.POL.MIL. EM SERVIÇO',
              'PESSOAS MORTAS POR POL. CIV. DE FOLGA',
              'PESSOAS MORTAS POR POL. CIV. DE FOLGA (7)',
              'PESSOAS MORTAS POR POL. CIV. DE FOLGA (8)',
              'PESSOAS MORTAS POR POL. CIV. DE FOLGA (9)',
              'PESSOAS MORTAS POR POL. CIV. DE FOLGA(9)',
              'PESSOAS MORTAS POR POL. MIL. DE FOLGA',
              'PESSOAS MORTAS POR POL. MIL. DE FOLGA (7)',
              'PESSOAS MORTAS POR POL. MIL. DE FOLGA (8)',
              'PESSOAS MORTAS POR POL. MIL. DE FOLGA (8A)',
              'PESSOAS MORTAS POR POL. MIL. DE FOLGA (9)',
              'POL. CIV. FERIDOS DE FOLGA',
              'POL. CIV. FERIDOS EM SERVIÇO',
              'POL. CIV. MORTOS DE FOLGA',
              'POL. CIV. MORTOS EM SERVIÇO',
              'POL. MIL. FERIDOS DE FOLGA',
              'POL. MIL. FERIDOS EM SERVIÇO',
              'POL. MIL. MORTOS DE FOLGA',
              'POL. MIL. MORTOS EM SERVIÇO',
              'PRISÕES EFETUADAS',
              'PRISÕES EFETUADAS (EM FLAGRANTE+ POR MANDADO)',
              'PRISÕES EFETUADAS(EM FLAGRANTE+PREVENTIVA+POR MANDADO)',
              'ROUBO',
              'ROUBO - OUTROS',
              'ROUBO - OUTROS (6)',
              'ROUBO - OUTROS (7)',
              'ROUBO - OUTROS TOTAL(6)',
              'ROUBO - OUTROS  (6)',
              'ROUBO - OUTROS(6)',
              'ROUBO - OUTROS(7)',
              'ROUBO A BANCO',
              'ROUBO DE CARGA',
              'ROUBO DE VEÍCULO',
              'ROUBO - OUTRO',
              'TENTATIVA DE HOMICÍDIO',
              'TOT. DE TERMOS CIRC. LAV.',
              'TOT. DE TERMOS CIRC. LAV. PELA POLÍCIA CIVIL (2)',
              'TOT. DE TERMOS CIRC. LAV. PELA POLÍCIA MILITAR',
              'TOT. INQ. POL. INSTAURADOS',
              'TOTAL DE BOLETINS DE OCORRÊNCIA',
              'TOTAL DE CRIMES VIOLENTOS ( HOM.DOLOSO, ROUBO, LATROCÍNIO, ESTUPRO E EMS)',
              'TOTAL DE CRIMES VIOLENTOS ( HOM0DOLOSO, ROUBO, LATROCÍNIO, ESTUPRO E EMS)',
              'TOTAL DE DELITOS',
              'TOTAL DE FURTO',
              'TOTAL DE INQUÉRITOS INSTAURADOS',
              'TOTAL DE ROUBO',
              'TRÁFICO DE ENTORPECENTES',
              'PESSOAS MORTAS EM CONF.POL.MIL. EM SERVIÇO (7)',
              'PESSOAS MORTAS EM CONF.POL.MIL. EM SERVIÇO (8)',
              'PESSOAS MORTAS EM CONF.POL.MIL. EM SERVIÇO (9)',
              'PESSOAS MORTAS EM CONF.POL.MIL. EM SERVIÇO(9)')
           
natureza2 <- c('t1',
               't2',
               't3',
               't4',
               't5',
               't4',
               't5',
               't3',
               't9',
               't10',
               't201',
               't201',
               't202',
               't203',
               't11',
               't11',
               't11',
               't12',
               't12',
               't12',
               't13',
               't14',
               't14',
               't14',
               't15',
               't15',
               't15',
               't15',
               't16',
               't17',
               't18',
               't19',
               't19',
               't19',
               't20',
               't19',
               't19',
               't21',
               't22',
               't22',
               't23',
               't24',
               't24',
               't25',
               't26',
               't26',
               't27',
               't27',
               't28',
               't29',
               't29',
               't30',
               't31',
               't32',
               't32',
               't33',
               't34',
               't34',
               't35',
               't36',
               't37',
               't38',
               't39',
               't40',
               't41',
               't42',
               't43',
               't44',
               't45',
               't46',
               't47',
               't47',
               't47',
               't48',
               't49',
               't50',
               't51',
               't51',
               't52',
               't53',
               't54',
               't55',
               't56',
               't57',
               't58',
               't59',
               't60',
               't61',
               't62',
               't62',
               't62',
               't62',
               't62',
               't63',
               't64',
               't65',
               't65',
               't65',
               't65',
               't65',
               't66',
               't66',
               't66',
               't66',
               't66',
               't67',
               't68',
               't69',
               't70',
               't71',
               't72',
               't73',
               't74',
               't75',
               't75',
               't75',
               't76',
               't77',
               't77',
               't77',
               't77',
               't77',
               't77',
               't77',
               't78',
               't79',
               't80',
               't81',
               't82',
               't83',
               't84',
               't85',
               't86',
               't87',
               't88',
               't88',
               't89',
               't16',
               't90',
               't76',
               't91',
               't92',
               't92',
               't92',
               't92')
}  

encode <- data.frame(natureza, natureza2)

base_tri <- left_join(base_tri, encode, by = "natureza")

aux <- subset.data.frame(base_tri,is.na(base_tri$natureza2) == TRUE)

Sys.sleep(10)

rm(encode)  

# 11° Passo: Excluindo coluna com o nome das variáveis e mantendo o código

base_tri <- base_tri %>%
  dplyr::select(-c(natureza))

# 12º Passo: Criando base no formato long

base_longa <- melt(base_tri, id=c("natureza2", "tri", "ano"))

Sys.sleep(10)

# 13º Passo: Codificando as regiões
{
variable <- c('kap',
              'gsp',
              'int',
              'dei1',
              'dei2',
              'dei3',
              'dei4',
              'dei5',
              'dei6',
              'dei7',
              'dei8',
              'dei9',
              'de10')
cod_reg <- c('10',
            '20',
            '30',
            '31',
            '32',
            '33',
            '34',
            '35',
            '36',
            '37',
            '38',
            '39',
            '40')
encode <- data.frame(variable, cod_reg)
}

base_longa <- left_join(base_longa, encode, by = "variable")

# 14° Passo: Excluindo coluna com o nome das regiões e mantendo o código
base_longa <- base_longa %>%
  dplyr::select(-c(variable))

# 15º Passo: Criando base no formato wyde
base_wyde <- base_longa %>%
  spread(key = "natureza2", value = "value")

Sys.sleep(10)

# 16º Passo: Reordenando as variáveis
base_wyde <- base_wyde %>%
  dplyr::select(cod_reg,
                ano, 
                tri,
                t1,
                t2,
                t3,
                t4,
                t5,
                t9,
                t10,
                t11,
                t12,
                t13,
                t14,
                t15,
                t16,
                t17,
                t18,
                t19,
                t20,
                t21,
                t22,
                t23,
                t24,
                t25,
                t26,
                t27,
                t28,
                t29,
                t30,
                t31,
                t32,
                t33,
                t34,
                t35,
                t36,
                t37,
                t38,
                t39,
                t40,
                t41,
                t42,
                t43,
                t44,
                t45,
                t46,
                t47,
                t48,
                t49,
                t50,
                t51,
                t52,
                t53,
                t54,
                t55,
                t56,
                t57,
                t58,
                t59,
                t60,
                t61,
                t62,
                t63,
                t64,
                t65,
                t66,
                t67,
                t68,
                t69,
                t70,
                t71,
                t72,
                t73,
                t74,
                t75,
                t76,
                t77,
                t78,
                t79,
                t80,
                t81,
                t82,
                t83,
                t84,
                t85,
                t86,
                t87,
                t88,
                t89,
                t90,
                t91,
                t92,
                t201,
                t202,
                t203)

write.table(base_wyde, 
            "base_trimestral_v3.csv", 
            sep = ";", 
            dec = ",", 
            row.names = FALSE)

if ( nrow(aux) == "0" ) {
  warning(paste("Não há variáveis novas a serem codificadas", sep = ""))
} else {
  warning(paste("As variáveis acima PRECISAM ser codificadas", sep = ""))
  table(aux$natureza) 
}



























