## Recodificação municipios 

library(tidyverse)
mdip_ssp <- readRDS("../data-raw/mdip_ssp.rds")
mdip_mp <- readRDS("../data-raw/mdip_mp.rds")


#base ssp
unique(mdip_ssp$municipio_circunscricao)
[1] "Adamantina"                              "Aguai"                                   "Aguas De S. Barbara"                    
[4] "Agudos"                                  "Altair"                                  "Alvares Machado"                        
[7] "Alvinlândia"                             "Americana"                               "Andradina"                              
[10] "Anhembi"                                 "Apiai"                                   "Aracariguama"                           
[13] "Aracatuba"                               "Aracoiaba Da Serra"                      "Arapei"                                 
[16] "Araraquara"                              "Araras"                                  "Ariranha"                               
[19] "Artur Nogueira"                          "Aruja"                                   "Assis"                                  
[22] "Atibaia"                                 "Auriflama"                               "Avare"                                  
[25] "Bady Bassit"                             "Barra Bonita"                            "Barra Do Turvo"                         
[28] "Barretos"                                "Barrinha"                                "Barueri"                                
[31] "Bastos"                                  "Batatais"                                "Bauru"                                  
[34] "Bebedouro"                               "Bertioga"                                "Birigui"                                
[37] "Biritiba-Mirim"                          "Bofete"                                  "Boituva"                                
[40] "Bom Jesus Dos Perdoes"                   "Botucatu"                                "Braganca Paulista"                      
[43] "Brotas"                                  "Buri"                                    "Buritama"                               
[46] "Cabrália Paulista"                       "Cacapava"                                "Cachoeira Paulista"                     
[49] "Cafelandia"                              "Caieiras"                                "Cajamar"                                
[52] "Cajati"                                  "Campinas"                                "Campo Limpo Paulista"                   
[55] "Campos Do Jordao"                        "Campos Novos Paul."                      "Cananeia"                               
[58] "Candido Mota"                            "Capao Bonito"                            "Capela Do Alto"                         
[61] "Capivari"                                "Caraguatatuba"                           "Carapicuiba"                            
[64] "Cardoso"                                 "Casa Branca"                             "Catanduva"                              
[67] "Catigua"                                 "Cedral"                                  "Cerquilho"                              
[70] "Cesario Lange"                           "Charqueada"                              "Conchal"                                
[73] "Conchas"                                 "Coronel Macedo"                          "Corumbatai"                             
[76] "Cosmopolis"                              "Cotia"                                   "Crime Militar"                          
[79] "Cristais Paulista"                       "Cruzeiro"                                "Cubatao"                                
[82] "Descalvado"                              "Diadema"                                 "Dumont"                                 
[85] "Eldorado"                                "Elias Fausto"                            "Elisiário"                              
[88] "Embu"                                    "Embu-Guacu"                              "Engenheiro Coelho"                      
[91] "Espirito Sto. Pinhal"                    "Fartura"                                 "Fernandopolis"                          
[94] "Ferraz De Vasconcelos"                   "Florida Paulista"                        "Franca"                                 
[97] "Francisco Morato"                        "Franco Da Rocha"                         "Gaviao Peixoto"                         
[100] "Gavião Peixoto"                          "Guaira"                                  "Guapiacu"                               
[103] "Guará"                                   "Guaracai"                                "Guararapes"                             
[106] "Guararema"                               "Guaratingueta"                           "Guarei"                                 
[109] "Guariba"                                 "Guaruja"                                 "Guarulhos"                              
[112] "Guzolândia"                              "Hortolandia"                             "Iacri"                                  
[115] "Ibaté"                                   "Ibira"                                   "Ibitinga"                               
[118] "Ibiuna"                                  "Iepê"                                    "Igarata"                                
[121] "Iguape"                                  "Ilha Comprida"                           "Ilhabela"                               
[124] "Indaiatuba"                              "Inubia Paulista"                         "Ipaussu"                                
[127] "Ipero"                                   "Iperó"                                   "Ipeuna"                                 
[130] "Ipigua"                                  "Iracemapolis"                            "Itai"                                   
[133] "Itajobi"                                 "Itanhaem"                                "Itapecerica Da Serra"                   
[136] "Itapetininga"                            "Itapeva"                                 "Itapevi"                                
[139] "Itapira"                                 "Itapirapua Paulista"                     "Itaquaquecetuba"                        
[142] "Itariri"                                 "Itatiba"                                 "Itirapina"                              
[145] "Itobi"                                   "Itu"                                     "Itupeva"                                
[148] "Ituverava"                               "Jaboticabal"                             "Jacarei"                                
[151] "Jaci"                                    "Jacupiranga"                             "Jaguariuna"                             
[154] "Jambeiro"                                "Jandira"                                 "Jardinopolis"                           
[157] "Jarinu"                                  "Jau"                                     "Jose Bonifacio"                         
[160] "Jundiai"                                 "Juquia"                                  "Juquitiba"                              
[163] "Laranjal Paulista"                       "Lavinia"                                 "Lavrinhas"                              
[166] "Leme"                                    "Lencois Paulista"                        "Limeira"                                
[169] "Lins"                                    "Lorena"                                  "Lourenço Da Serra"                      
[172] "Louveira"                                "Macatuba"                                "Mairinque"                              
[175] "Mairipora"                               "Manduri"                                 "Maracai"                                
[178] "Marilia"                                 "Martinopolis"                            "Matao"                                  
[181] "Maua"                                    "Meridiano"                               "Mineiros Do Tiete"                      
[184] "Miracatu"                                "Mirassol"                                "Mococa"                                 
[187] "Mogi-Guacu"                              "Mogi-Mirim"                              "Mogi Das Cruzes"                        
[190] "Mombuca"                                 "Mongagua"                                "Monte Aprazivel"                        
[193] "Monte Mor"                               "Morro Agudo"                             "Murutinga Do Sul"                       
[196] "Nazare Paulista"                         "Neves Paulista"                          "Nova Alianca"                           
[199] "Nova Europa"                             "Nova Granada"                            "Nova Odessa"                            
[202] "Nuporanga"                               "Olimpia"                                 "Onda Verde"                             
[205] "Orlandia"                                "Osasco"                                  "Ourinhos"                               
[208] "Palestina"                               "Palmital"                                "Panorama"                               
[211] "Paraguacu Paulista"                      "Pariquera-Acu"                           "Patrocinio Paulista"                    
[214] "Pauliceia"                               "Paulinia"                                "Paulo De Faria"                         
[217] "Pederneiras"                             "Pedregulho"                              "Pedro De Toledo"                        
[220] "Peruibe"                                 "Piedade"                                 "Pilar Do Sul"                           
[223] "Pindamonhangaba"                         "Piquete"                                 "Piracaia"                               
[226] "Piracicaba"                              "Piraju"                                  "Pirapora Do Bom Jesus"                  
[229] "Pirapozinho"                             "Pitangueiras"                            "Poa"                                    
[232] "Policia Federal"                         "Polícia Federal Inquérito Da Pf 1082/14" "Polícia Federal Inquérito Da Pf 1658/14"
[235] "Poloni"                                  "Pongai"                                  "Pontal"                                 
[238] "Porto Feliz"                             "Porto Ferreira"                          "Potirendaba"                            
[241] "Pracinha"                                "Pradopolis"                              "Praia Grande"                           
[244] "Presidente Bernardes"                    "Presidente Epitacio"                     "Presidente Prudente"                    
[247] "Quata"                                   "Registrado Em (Mg)"                      "Registrado Na Pf"                       
[250] "Registrado Na Pf - Bo 1114/16"           "Registrado Na Pf - Bo 200/15"            "Registrado Na Pf - Bo 206/17"           
[253] "Registro"                                "Ribeirao Branco"                         "Ribeirao Grande"                        
[256] "Ribeirao Pires"                          "Ribeirao Preto"                          "Rifaina"                                
[259] "Rincao"                                  "Rinopolis"                               "Rio Claro"                              
[262] "Rio Das Pedras"                          "Rio Grande Da Serra"                     "Rosana"                                 
[265] "Rubiacea"                                "S.andre"                                 "S.barbara D Oeste"                      
[268] "S.bernardo Do Campo"                     "S.caetano Do Sul"                        "S.carlos"                               
[271] "S.cruz Da Conceicao"                     "S.cruz Das Palmeiras"                    "S.cruz Do Rio Pardo"                    
[274] "S.gertrudes"                             "S.isabel"                                "S.joaquim Da Barra"                     
[277] "S.jose Do Rio Preto"                     "S.jose Dos Campos"                       "S.luis Do Paraitinga"                   
[280] "S.miguel Arcanjo"                        "S.paulo"                                 "S.pedro"                                
[283] "S.roque"                                 "S.rosa De Viterbo"                       "S.sebastiao"                            
[286] "S.simao"                                 "S.vicente"                               "Sabino"                                 
[289] "Sales Oliveira"                          "Salesopolis"                             "Saltinho"                               
[292] "Salto"                                   "Salto De Pirapora"                       "Santa Maria Da Serra"                   
[295] "Santana De Parnaiba"                     "Santos"                                  "Sao Joao Da Boa Vista"                  
[298] "São Manuel"                              "Serra Azul"                              "Serra Negra"                            
[301] "Serrana"                                 "Sertaozinho"                             "Sete Barras"                            
[304] "Socorro"                                 "Sorocaba"                                "Sumare"                                 
[307] "Suzano"                                  "Taboao Da Serra"                         "Tanabi"                                 
[310] "Tapirai"                                 "Taquaritinga"                            "Taquarituba"                            
[313] "Tatui"                                   "Taubate"                                 "Terra Roxa"                             
[316] "Tiete"                                   "Torre De Pedra"                          "Torrinha"                               
[319] "Tremembe"                                "Tupa"                                    "Ubatuba"                                
[322] "Uchoa"                                   "Valentim Gentil"                         "Valinhos"                               
[325] "Vargem"                                  "Vargem Grande Paulista"                  "Varzea Paulista"                        
[328] "Vinhedo"                                 "Viradouro"                               "Votorantim"                             
[331] "Votuporanga"    

#base mp
unique(mdip_mp$cidade)

[1] "Adamantina"               "Aguaí"                    "Águas De Lindóia"         "Águas De Santa Bárbara"  
[5] "Agudos"                   "Altair"                   "Altinópolis"              "Álvares Machado"         
[9] "Alvinlândia"              "Americana"                "Anhembi"                  "Apiaí"                   
[13] "Araçariguama"             "Araçatuba"                "Araçoiaba Da Serra"       "Arapeí"                  
[17] "Araraquara"               "Araras"                   "Ariranha"                 "Artur Nogueira"          
[21] "Arujá"                    "Assis"                    "Atibaia"                  "Avaré"                   
[25] "Bady Bassitt"             "Barra Do Turvo"           "Barretos"                 "Barrinha"                
[29] "Barueri"                  "Bastos"                   "Batatais"                 "Bauru"                   
[33] "Bebedouro"                "Bernardino De Campos"     "Bertioga"                 "Birigui"                 
[37] "Biritiba Mirim"           "Bofete"                   "Boituva"                  "Bom Jesus Dos Perdões"   
[41] "Borborema"                "Botucatu"                 "Bragança Paulista"        "Buri"                    
[45] "Buritama"                 "Cabrália Paulista"        "Caçapava"                 "Caieiras"                
[49] "Cajamar"                  "Cajati"                   "Campinas"                 "Campo Limpo Paulista"    
[53] "Campos Do Jordão"         "Campos Novos Paulista"    "Campos Novos Paulistas"   "Cananéia"                
[57] "Cândido Mota"             "Capela Do Alto"           "Capivari"                 "Caraguatatuba"           
[61] "Carapicuíba"              "Casa Branca"              "Catiguá"                  "Cedral"                  
[65] "Cerqueira César"          "Cerquilho"                "Charqueada"               "Conchal"                 
[69] "Conchas"                  "Coronel Macedo"           "Corumbataí"               "Cosmópolis"              
[73] "Cotia"                    "Cruzeiro"                 "Cubatão"                  "Descalvado"              
[77] "Diadema"                  "Dracena"                  "Elias Fausto"             "Elisiário"               
[81] "Embu-Guaçu"               "Embu Das Artes"           "Engenheiro Coelho"        "Espírito Santo Do Pinhal"
[85] "Fartura"                  "Fernandópolis"            "Ferraz De Vasconcelos"    "Flórida Paulista"        
[89] "Franca"                   "Francisco Morato"         "Franco Da Rocha"          "Gavião Peixoto"          
[93] "Guaíra"                   "Guapiaçu"                 "Guararapes"               "Guararema"               
[97] "Guaratinguetá"            "Guareí"                   "Guarujá"                  "Guarulhos"               
[101] "Guzolândia"               "Hortolândia"              "Iacanga"                  "Iacri"                   
[105] "Ibaté"                    "Ibirá"                    "Ibiuna"                   "Ibiúna"                  
[109] "Iepê"                     "Igaratá"                  "Ilha Comprida"            "Indaiatuba"              
[113] "Inúbia Paulista"          "Iperó"                    "Itaí"                     "Itanhaém"                
[117] "Itapecerica Da Serra"     "Itapetininga"             "Itapevi"                  "Itapira"                 
[121] "Itaquaquecetuba"          "Itariri"                  "Itatiba"                  "Itirapina"               
[125] "Itobi"                    "Itu"                      "Itupeva"                  "Jaboticabal"             
[129] "Jacareí"                  "Jaci"                     "Jacupiranga"              "Jaguariúna"              
[133] "Jambeiro"                 "Jandira"                  "Jardinópolis"             "Jarinu"                  
[137] "Jaú"                      "Jundiaí"                  "Juquitiba"                "Laranjal Paulista"       
[141] "Lavrinhas"                "Leme"                     "Lençóis Paulista"         "Limeira"                 
[145] "Lorena"                   "Louveira"                 "Macatuba"                 "Mairinque"               
[149] "Mairiporã"                "Maracaí"                  "Marília"                  "Martinópolis"            
[153] "Matão"                    "Mauá"                     "Meridiano"                "Mineiros Do Tietê"       
[157] "Miracatu"                 "Mogi Das Cruzes"          "Mogi Guaçu"               "Mogi Mirim"              
[161] "Mombuca"                  "Mongaguá"                 "Monte Aprazível"          "Monte Mor"               
[165] "Morro Agudo"              "Nazaré Paulista"          "Neves Paulista"           "Nova Aliança"            
[169] "Nova Odessa"              "Nuporanga"                "Olímpia"                  "Onda Verde"              
[173] "Orlândia"                 "Osasco"                   "Osvaldo Cruz"             "Ourinhos"                
[177] "Panorama"                 "Pariquera-Açu"            "Paulicéia"                "Paulínia"                
[181] "Paulo De Faria"           "Pederneiras"              "Pedregulho"               "Pedro De Toledo"         
[185] "Peruíbe"                  "Pilar Do Sul"             "Pindamonhangaba"          "Pinhalzinho"             
[189] "Piquete"                  "Piracicaba"               "Pirapora Do Bom Jesus"    "Pirassununga"            
[193] "Pitangueiras"             "Poá"                      "Poloni"                   "Pongaí"                  
[197] "Pontal"                   "Porto Feliz"              "Porto Ferreira"           "Pracinha"                
[201] "Pradópolis"               "Praia Grande"             "Presidente Bernardes"     "Presidente Prudente"     
[205] "Quatá"                    "Queluz"                   "Registro"                 "Ribeirão Branco"         
[209] "Ribeirão Grande"          "Ribeirão Pires"           "Ribeirão Preto"           "Rifaina"                 
[213] "Rincão"                   "Rinópolis"                "Rio Claro"                "Rio Grande Da Serra"     
[217] "Rosana"                   "Sabino"                   "Sales Oliveira"           "Salesópolis"             
[221] "Salto"                    "Salto De Pirapora"        "Santa Bárbara D'oeste"    "Santa Cruz Das Palmeiras"
[225] "Santa Fé Do Sul"          "Santa Isabel"             "Santana De Parnaíba"      "Santo André"             
[229] "Santos"                   "São Bernardo Do Campo"    "São Caetano Do Sul"       "São Carlos"              
[233] "São José Do Rio Preto"    "São José Dos Campos"      "São Lourenço Da Serra"    "São Manuel"              
[237] "São Paulo"                "São Pedro"                "São Roque"                "São Sebastião"           
[241] "São Simão"                "São Vicente"              "Serra Negra"              "Serrana"                 
[245] "Sertãozinho"              "Sete Barras"              "Socorro"                  "Sorocaba"                
[249] "Sumaré"                   "Suzano"                   "Taboão Da Serra"          "Tanabi"                  
[253] "Taquaritinga"             "Taquarituba"              "Tatuí"                    "Taubaté"                 
[257] "Torrinha"                 "Tremembé"                 "Tupã"                     "Ubatuba"                 
[261] "Valinhos"                 "Vargem Grande Paulista"   "Várzea Paulista"          "Votorantim"              
[265] "Votuporanga"        

#código 

mdip_ssp %>%  mutate (municipio_limpo = case_when(
  municipio_circunscricao %in% c("S.PAULO","SÃO PAULO") ~ "São Paulo",
  municipio_circunscricao %in% c("ORLANDIA", "ORLÂNDIA") ~ "nome_novo",
  municipio_circunscricao == "AGUAÍ"~ "Aguaí",
  municipio_circunscricao == "ÁGUAS DE LINDÓIA"~"Águas de Lindóia",
  municipio_circunscricao == "Aguai" ~ "Aguaí",
  municipio_circunscricao %in% c("Aguas De S. Barbara", "Águas De Santa Bárbara") ~ "Águas de Santa Bárbara",
  municipio_circunscricao == "Águas De Lindóia" ~ "Águas de Lindóia",
  municipio_circunscricao == "Apiai" ~ "Apiaí",
  municipio_circunscricao == "Aracariguama" ~ "Araçariguama",
  municipio_circunscricao == "Aracatuba" ~ "Araçatuba"
  municipio_circunscricao %in% c("Aracoiaba Da Serra", "Araçoiaba Da Serra") ~ "Araçoiaba da Serra",
  municipio_circunscricao == "Arapei" ~ "Arapeí",
  municipio_circunscricao == "Aruja" ~ "Arujá",
  municipio_circunscricao == "Avare" ~ "Avaré",
  municipio_circunscricao == "Bady Bassit" ~ "Bady Bassitt",
  municipio_circunscricao == "Barra Do Turvo" ~ "Barra do Turvo",
  municipio_circunscricao == "Bernardino De Campos" ~ "Bernardino de Campos",
  municipio_circunscricao == "Biritiba-Mirim" ~ "Biritiba Mirim",
  municipio_circunscricao %in% c("Bom Jesus Dos Perdoes", "Bom Jesus Dos Perdões") ~ "Bom Jesus dos Perdões",
  municipio_circunscricao == "Braganca Paulista" ~ "Bragança Paulista",
  municipio_circunscricao == "Cacapava" ~ "Caçapava",
  municipio_circunscricao == "Cafelandia" ~ "Cafelândia",
  municipio_circunscricao %in% c("Campos Do Jordao", "Campos Do Jordão") ~ "Campos do Jordão",
  municipio_circunscricao == "Campos Novos Paul." ~ "Campos Novos Paulista",
  municipio_circunscricao == "Cananeia" ~ "Cananéia",
  municipio_circunscricao == "Candido Mota" ~ "Cândido Mota",
  municipio_circunscricao == "Capao Bonito" ~ "Capão Bonito",
  municipio_circunscricao %in% c("Capela Do Alto") ~ "Capela do Alto",
  municipio_circunscricao == "Carapicuiba" ~ "Carapicuíba",
  municipio_circunscricao == "Catigua" ~ "Catiguá",
  municipio_circunscricao == "Cesario Lange" ~ "Cesário Lange",
  municipio_circunscricao == "Corumbatai" ~ "Corumbataí",
  municipio_circunscricao == "Cosmopolis" ~ "Cosmópolis",
  municipio_circunscricao == "Cubatao" ~ "Cubatão",
  #municipio_circunscricao == ou %in% "nome_antigo" ~"nome_novo",
  
  

  
  TRUE = as.character(cidade)
))


