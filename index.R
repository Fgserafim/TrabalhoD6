## Título: Sistema Nacional de Cultura - adesões pelos entes federados

# Autor: Flávia Gonzaga Serafim

## Objeto: 
# Conforme arquivo index.rmd

## Objetivos principais e justificativas
# Conforme arquivo index.rmd

## Método
# Conforme arquivo index.rmd

## Instalação dos pacotes

lista.de.pacotes = c("tidyverse", "magrittr", "lubridate", "ggplot2", "dplyr", 
                     "openxlsx", "readr", "readxl", "xlsx")
novos.pacotes <- lista.de.pacotes[!(lista.de.pacotes %in%
                                      installed.packages()[,"Package"])]
if(length(novos.pacotes) > 0) {install.packages(novos.pacotes)}
lapply(lista.de.pacotes, require, character.only=T)
rm(lista.de.pacotes,novos.pacotes)
gc()

## Base de dados "Datas"

# Leitura da base de dados "Datas"

datas <- read.xlsx("E:/D6/Dados_trabalho/Datas.xlsx", detectDates=T)
str(datas)

datas <- datas %>% # converter de caracter em data 
  mutate(Data_adesao = as_date(Data_adesao))
str(datas)
View(datas)

# Quantidade de entes federados que aderiram ao SNC

entes_adesao <- datas %>%
  filter(!is.na(Aderiu_ao_SNC)) %>% # exclui as observações "NA"
  count(Aderiu_ao_SNC) # conta a quantidade de entes que se integraram ao SNC
str(entes_adesao)

# Quantidade de municípios por estado que aderiram ao SNC

Munic_Estado <- datas %>%
  filter(!is.na(Aderiu_ao_SNC)) %>%
  group_by(Estado) %>% # agrupa por Estado e conta os municípios pelo grupo
  count(Estado) 
head(Munic_Estado)
View(Munic_Estado)

# Total da população nos estados que aderiram ao SNC

Pop_estado_aderiu <- datas %>%
  filter(!is.na(Aderiu_ao_SNC)) %>%
  select(Estado, Estado_Municipio, Pop_estimada_2017, Aderiu_ao_SNC) %>%
  group_by(Estado) %>% # agrupa por Estado e soma a população pelo grupo
  summarise(sum(Pop_estimada_2017))
head(Pop_estado_aderiu)
View(Pop_estado_aderiu)

# Quantidade de adesões por ano

adesoes_ano <- datas %>%
  filter(!is.na(Aderiu_ao_SNC)) %>% # exclui "NA"
  filter(!is.na(Data_adesao)) %>% # exclui "NA"
  select(Estado, Estado_Municipio, Data_adesao) %>%
  mutate(ano_adesao = year(ymd(Data_adesao))) %>% # converte a abservação ano/mês/dia em ano
  group_by(ano_adesao) %>% # agrupa por ano
  summarise(quantidade_adesoes = n()) # conta a quantidade de adesões pelo grupo
str(adesoes_ano)
View(adesoes_ano)

# Gráfico 1: número de adesões ao SNC por ano

ggplot(adesoes_ano) + 
  geom_bar(aes(x = ano_adesao, y = quantidade_adesoes, color=quantidade_adesoes, fill=quantidade_adesoes), stat = "identity") +
  labs(title="Adesões anuais ao SNC", x="ano da adesao", y="quantidade de adesões")

## Base de dados "Adesao"

# Leitura da base de dados "Adesao"

adesao <- read_xlsx("E:/D6/Dados_trabalho/Adesao.xlsx")
str(adesao)
View(adesao)

# Quantidade de entes federados em relação à situação da adesão ao SNC

situacao <- adesao %>%
  group_by(Situação) %>% # agrupa por situação da integração ao SNC
  count(Situação) # conta as observações pelo grupo
situacao

# Quantidade de entes federados com adesao publicada e que constituíram sistema local de cultura

publicado <-  adesao %>%
  select(UF, Ente, Situação, Possui_Sistema) %>%
  filter(Situação == "Publicado no DOU" & Possui_Sistema == "Sim") %>% # filtra observando as duas condições
  count(Possui_Sistema) # conta considerando o filtro
publicado

# Quantidade de entes federados com adesão publicada e que possuem órgão gestor local

org_gestor <- adesao %>%
  select(UF, Ente, Situação, Possui_Órgão_Gestor) %>%
  filter(Situação == "Publicado no DOU" & Possui_Órgão_Gestor == "Sim") %>% # filtra observando as duas condições
  count(Possui_Órgão_Gestor) # conta considerando o filtro
org_gestor

# Quantidade de entes federados com adesao publicada e que instituíram Conselho de política cultural

conselho <- adesao %>%
  select(UF, Ente, Situação, Possui_Conselho) %>%
  filter(Situação == "Publicado no DOU" & Possui_Conselho == "Sim") %>% # filtra observando as duas condições
  count(Possui_Conselho) # conta considerando o filtro
conselho

# Quantidade de entes federados com adesao publicada e que definiram mecanismo para fomento da cultura

mec_fomento <- adesao %>%
  select(UF, Ente, Situação, Possui_Mecanismo_Fomento) %>%
  filter(Situação == "Publicado no DOU" & Possui_Mecanismo_Fomento == "Sim") %>% # filtra observando as duas condições
  count(Possui_Mecanismo_Fomento) # conta considerando o filtro
mec_fomento

# Quantidade de entes federados com adesão publicada e que elaboraram plano de política cultural

plano <- adesao %>%
  select(UF, Ente, Situação, Possui_Plano) %>%
  filter(Situação == "Publicado no DOU" & Possui_Plano == "Sim") %>% # filtra observando as duas condições
  count(Possui_Plano) # conta considerando o filtro
plano

# Quantidade de municípios por estado com adesão publicada e todos os componentes definidos

adesao_compl <- adesao %>%
  select(UF:Possui_Plano, -Codigo) %>%
  mutate(Situação == "Publicado no DOU", Possui_Sistema == "Sim", Possui_Órgão_Gestor == "Sim", 
         Possui_Conselho == "Sim", Possui_Mecanismo_Fomento == "Sim", Possui_Plano == "Sim") %>% # seleciona os municípios com todos os componentes
  group_by(UF) %>% # agrupa por estado
  count(UF) # conta municípios pelo grupo
glimpse(adesao_compl)
adesao_compl

# Gráfico 2: quantidade de municípios por estado com todos os componentes do sistema definidos

ggplot(adesao_compl) +
  geom_bar(aes(x= UF, y = n, color = n, fill = n), stat = "identity") +
  labs(title="MunicÃ?pios com todos os componentes do SNC", x="estados", y="número de municípios") +
  coord_flip()

# Quantidade de municípios com todos os componentes definidos, em sua região

adesao_regiao <- adesao_compl %>%
  mutate(Regiao = 
           ifelse(UF %in% c("AC", "RO", "RR", "AM", "PA", "TO", "AP"), "Norte", 
                  ifelse(UF %in% c("MA", "PI", "CE", "RN", "PE", "PB", "SE", "AL", "BA"), "Nordeste",
                         ifelse(UF %in% c("MT", "MS", "GO", "DF"), "Centro-Oeste", 
                                ifelse(UF %in% c("SP", "RJ", "ES", "MG"), "Sudeste", "Sul"))))) # cria coluna com a região que o estado pertence
adesao_regiao

# Gráfico 3: Número de municípios nos estados com todos os componentes definidos, em sua região

ggplot(adesao_regiao) +
  geom_bar(aes(x= UF, y = n, color=Regiao, fill=Regiao), stat = "identity") +
  labs(title="Municípios com todos os componentes do sistema, por região", 
       x = "estados por região", y = "número  de municípios") +
  coord_flip()

# Gráfico 4: Número de municípios nos estado, por região, com todos os componentes definidos

ggplot(adesao_regiao, aes(x = Regiao,  weights = n, stat = "identity")) +
  geom_bar(aes(fill = UF), color="Black") +
  geom_text(aes(x = Regiao, y = n, group = UF, label = UF),
            position = position_stack(vjust = 0.5), size=2) +
  guides(fill=FALSE) +
  xlab("Região do Brasil") + ylab("Número de municípios") +
  labs(title="Municípios com todos os componentes, por região")

# Percentual de municípios nos estados com todos os componentes do sistema de cultura local definidos

adesao_regiao_agrupada <- adesao_regiao %>%
  group_by(Regiao) %>% # agrupa por região
  summarise(contagem = sum(n)) # soma pelo grupo
adesao_regiao_agrupada

total_agrup <- adesao_regiao_agrupada %>%
  select(Regiao, contagem) %>%
  mutate(total_mun = c(467, 1794, 450, 1668, 1191)) # insere coluna contendo o número total de municípios por região (IBGE)
total_agrup

total_mun_percent <- total_agrup %>%
  select(Regiao, contagem, total_mun) %>%
  group_by(Regiao) %>% # agrupa por região
  mutate(percentual = contagem / total_mun * 100) # calcula e cria coluna com o percentual de municípios por região
total_mun_percent

## Conclusões
# Conforme arquivo index.rmd
