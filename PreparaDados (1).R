rm(list=ls())
memory.limit()
memory.limit(size=3000000000)
######Prepara dados para rodar simulaÃƒÂ§ÃƒÂ£o sem usar shyni. 

#------------------------ imports ------------------------
{
library("readxl")
library("doParallel")
library("stringr")
library("compiler")
library("ggplot2")
library(dplyr)
library(tidyr)
}
#----------------------- carregar funÃƒÂ§ÃƒÂµes -----------------
##Indica diretÃƒÂ³rio onde salvou as funÃƒÂ§ÃƒÂµes
setwd("C:\\Users\\User\\Dropbox\\MESTRADO EM DEMOGRAFIA\\Textos - Dissertação\\Capacidade de contratação dos municípios\\Dados\\RAIS - 01-07-2020\\Dados\\Funcoes (1)")
{
  source("calculaValoresPagamento.R")
  source("calculaValoresPagamentoAberta.R")
  source("estimaTempoAteMorte.R")
  source("estimaTempoAteSaida.R")
  source("EstimaTx.R")
  source("geraGraficoPercentil.R")
  source("gerarLx.R")
  source("gerarTDU.R")
  source("gerarTMDTDU.R")
  source("idadeTipoAposentadoria.R")
  source("lancarErro.r")
  source("piramide.R")
  source("resumeEstadoServidor.R")
#  source("resumeEstadoServidorAberta.R")
  source("rodaSimulacao.R")
 # source("rodaSimulacaoAberta.R")
  #source("estimaIdadeAposentadoriaRPPS.R")
 # source("estimaIdadeAposentadoriaRGPS.R")
  source("EstimaPopEntrada.R")
  #source("rodaSimulacaoAbertaGrande.R")
  #source("calculaValoresPagamentoAberta")
}



##Define parÃ¢metros
{
  Diretorio="C:\\Users\\User\\Dropbox\\MESTRADO EM DEMOGRAFIA\\Textos - Dissertação\\Capacidade de contratação dos municípios\\Dados\\RAIS - 01-07-2020\\Dados\\Funcoes (1)"      
  TetoINSS=5645.80   ###Verificar valor
  Tempo=50
  Rodadas=2500  ##Mude 100 para testar, mas os resultados oficiais devem ser com quantidade maior, de pelo menos 2500 rodadas para municÃƒï¿½pios e 1000 para estados.  
  SalMinimo = 954 #estabelece salario minimo de 954
  
  }



##Carrega dados inciais
DiretorioDadosIniciais="C:\\Users\\User\\Dropbox\\MESTRADO EM DEMOGRAFIA\\Textos - Dissertação\\Capacidade de contratação dos municípios\\Dados\\RAIS - 01-07-2020\\Dados\\Funcoes (1)\\Dados"
DiretorioGrardaResultados="C:\\Users\\User\\Dropbox\\MESTRADO EM DEMOGRAFIA\\Textos - Dissertação\\Capacidade de contratação dos municípios\\Dados\\RAIS - 01-07-2020\\Dados\\Funcoes (1)\\Resultados"


{
setwd(DiretorioDadosIniciais)
tabelaInvalidez=read.csv("AlvaroVindas.csv", sep=";", dec=",")
mun_grupo <-read.csv("municipio_grupomuniciElaine.csv", sep=",")
TxSalarial=read.csv("AumentoSalarialPorEnte.csv", sep=";", dec=",")
}


dados = data.frame()

for(i in 93:5548) {
  row <- mun_grupo[i,]
  Municipio= toString(row[1])
  #Grupo = toString(row[2])
  
  Dados=read.csv(paste0(DiretorioDadosIniciais, "\\Populacao Inicial\\",Municipio," .csv"), sep=",", dec=".")
  
  Dados=Dados[Dados$EstadoInicial==1,]   ##Para este projeto sÃ³ interessam os ativos
  taxaAumentoSalarial=TxSalarial$taxaAumentoSalarial[TxSalarial$NomeMunicipio==Municipio]
  tabelaMortalidade= read.csv(paste0(DiretorioDadosIniciais,"\\Tabuas\\TabelaVida ",Municipio," .csv"), sep=",", dec=".")
  #df =  rodaSimulacao (taxaAumentoSalarial, DadosServidores, Diretorio, Rodadas, Tempo, tabelaMortalidade, tabelaInvalidez,  Municipio,TetoINSS)
  
  Categoria = unique(Dados$Categoria)
  
  for(c in Categoria){
    
    DadosServidores = Dados[Dados$Categoria==c,]
    
    da =  rodaSimulacao (taxaAumentoSalarial, DadosServidores, Diretorio, Rodadas, Tempo, tabelaMortalidade, tabelaInvalidez,  Municipio,TetoINSS)
    
    dado = data.frame("Ano" = da[1],
                      "Municipio" = Municipio,
                      "Categoria"=c,
                      "Minimo"=da[2],
                      "IC  0.95 % Menor"=da[3],
                      "Media"=da[4],
                      "IC  0.95 % Maior"=da[5],
                      "Maximo"=da[6],
                      "Tempo medio ate saida para aposentadoria"  = da[7])
    dados = rbind(dados,dado)  
  }
  
  write.csv(dados,paste0(DiretorioGrardaResultados,"/Resultados Populacao Fechada e Aberta.csv"), row.names=FALSE)
  
  dados2020 = dados %>% filter(Ano==2020) %>% select (Ano, Municipio, Categoria, Media)%>%
    group_by(Municipio, Categoria) %>% pivot_wider(names_from = Categoria, values_from = Media)
  write.csv(dados2020,paste0(DiretorioGrardaResultados,"/Resultados Populacao Fechada e Aberta2020.csv"), row.names = FALSE)
  
  dados2025= dados %>% filter(Ano==2025) %>% select (Ano, Municipio, Categoria, Media)%>%
    group_by(Municipio, Categoria) %>% pivot_wider(names_from = Categoria, values_from = Media)
  write.csv(dados2025,paste0(DiretorioGrardaResultados,"/Resultados Populacao Fechada e Aberta2025.csv"), row.names = FALSE)
  
  dados2030= dados %>% filter(Ano==2030) %>% select (Ano, Municipio, Categoria, Media)%>%
    group_by(Municipio, Categoria) %>% pivot_wider(names_from = Categoria, values_from = Media)
  write.csv(dados2030,paste0(DiretorioGrardaResultados,"/Resultados Populacao Fechada e Aberta2030.csv"), row.names = FALSE)
  
  print(i)
}


# Quando der erro, fechar o pgm e atualizar a área de trabalho do pc
   
#OS QUE NÃO RODARAM: 6,27,44,45,57,58,92 - educacao, 110 - outros,
#





####Para munic?pios grandes




Rodadas=1
#for(i in 1:11) {
i=1
  row <- mun_grupo[i,]
  Municipio= toString(row[1])
  Grupo = toString(row[2])
  #DadosServidores=read.csv(paste0(DiretorioDadosIniciais,"/Populacao Inicial grupo ",Municipio,".csv"), sep=",")
  DadosServidores=read.csv(paste0(DiretorioDadosIniciais,"/Populacao Inicial ",Municipio," .csv"), sep=",")
  DadosServidores=DadosServidores[DadosServidores$EstadoInicial==1,]   ##Para este projeto sÃ³ interessam os ativos
  taxaAumentoSalarial=TxSalarial$taxaAumentoSalarial[TxSalarial$NomeMunicipio==Municipio]
  tabelaMortalidade= read.csv(paste0(DiretorioDadosIniciais,"/TabelaVidaGrupo",row[2],".csv"), sep=";", dec=",")
  #df =  rodaSimulacao (taxaAumentoSalarial, DadosServidores, Diretorio, Rodadas, Tempo, tabelaMortalidade, tabelaInvalidez,  Municipio,TetoINSS)
  da =  rodaSimulacaoAberta (taxaAumentoSalarial, DadosServidores, Diretorio, Rodadas, Tempo, tabelaMortalidade, tabelaInvalidez,  Municipio,TetoINSS, SalMinimo)
  
  dado = data.frame("Municipio" = Municipio, 
                    "Grupo" = Grupo, 
                    "Mediana da Soma dos Salarios de ContribuiÃ§Ã£o Acima do Teto Projetados" = da[1],
                    "Mediana da Soma dos Salarios de ContribuiÃ§Ã£o Projetados" = da[2],
                    "Tempo medio ate saida para aposentadoria"  = da[3], 
                    "Mediana da Soma dos Salarios de Contribuicao Acima do Teto Projetados Pop Aberta"=da[4],
                    "Mediana da Soma dos Salarios de ContribuiÃ§Ã£o Projetados Pop Aberta"=da[5], 
                    "NÃºmero mediano de pessoas recebendo acima do teto do INSS"=da[6])
  dados = rbind(dados,dado)
  write.csv(dados,paste0(DiretorioGrardaResultados,"\\Resultados Populacao Fechada e Aberta.csv"))
  print(i)
#}
