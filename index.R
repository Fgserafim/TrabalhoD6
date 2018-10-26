## Titulo: Sistema Nacional de Cultura - adesoes pelos entes federados

# Autor: Flavia Gonzaga Serafim

## Objeto: 
# Conforme arquivo index.rmd

## Objetivos principais e justificativas
# Conforme arquivo index.rmd

## Metodo
# Conforme arquivo index.rmd

## Instalacao dos pacotes

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
  filter(!is.na(Aderiu_ao_SNC)) %>% # exclui as observacoes "NA"
  count(Aderiu_ao_SNC) # conta a quantidade de entes que se integraram ao SNC
str(entes_adesao)

# Quantidade de municipios por estado que aderiram ao SNC

Munic_Estado <- datas %>%
  filter(!is.na(Aderiu_ao_SNC)) %>%
  group_by(Estado) %>% # agrupa por Estado e conta os municipios pelo grupo
  count(Estado) 
head(Munic_Estado)
View(Munic_Estado)

# Total da populacao nos estados que aderiram ao SNC

Pop_estado_aderiu <- datas %>%
  filter(!is.na(Aderiu_ao_SNC)) %>%
  select(Estado, Estado_Municipio, Pop_estimada_2017, Aderiu_ao_SNC) %>%
  group_by(Estado) %>% # agrupa por Estado e soma a populacao pelo grupo
  summarise(sum(Pop_estimada_2017))
head(Pop_estado_aderiu)
View(Pop_estado_aderiu)

# Quantidade de adesoes por ano

adesoes_ano <- datas %>%
  filter(!is.na(Aderiu_ao_SNC)) %>% # exclui "NA"
  filter(!is.na(Data_adesao)) %>% # exclui "NA"
  select(Estado, Estado_Municipio, Data_adesao) %>%
  mutate(ano_adesao = year(ymd(Data_adesao))) %>% # converte a abservacao ano/mes/dia em ano
  group_by(ano_adesao) %>% # agrupa por ano
  summarise(quantidade_adesoes = n()) # conta a quantidade de adesoes pelo grupo
str(adesoes_ano)
View(adesoes_ano)

# Grafico 1: numero de adesoes ao SNC por ano

ggplot(adesoes_ano) + 
  geom_bar(aes(x = ano_adesao, y = quantidade_adesoes, color=quantidade_adesoes, fill=quantidade_adesoes), stat = "identity") +
  labs(title="Adesoes anuais ao SNC", x="ano da adesao", y="quantidade de adesoes")

## Base de dados "Adesao"

# Leitura da base de dados "Adesao"

adesao <- read_xlsx("E:/D6/Dados_trabalho/Adesao.xlsx")
str(adesao)
View(adesao)

# Quantidade de entes federados em relacao a situacao da adesao ao SNC

situacao <- adesao %>%
  group_by(SituaÃÂ§ÃÂ£o) %>% # agrupa por situacao da integracao ao SNC
  count(SituaÃÂ§ÃÂ£o) # conta as observacoes pelo grupo
situacao

# Quantidade de entes federados com adesao publicada e que constituiram sistema local de cultura

publicado <-  adesao %>%
  select(UF, Ente, SituaÃÂ§ÃÂ£o, Possui_Sistema) %>%
  filter(SituaÃÂ§ÃÂ£o == "Publicado no DOU" & Possui_Sistema == "Sim") %>% # filtra observando as duas condicoes
  count(Possui_Sistema) # conta considerando o filtro
publicado

# Quantidade de entes federados com adesao publicada e que possuem orgao gestor local

org_gestor <- adesao %>%
  select(UF, Ente, SituaÃÂ§ÃÂ£o, Possui_ÃÂrgÃÂ£o_Gestor) %>%
  filter(SituaÃÂ§ÃÂ£o == "Publicado no DOU" & Possui_ÃÂrgÃÂ£o_Gestor == "Sim") %>% # filtra observando as duas condicoes
  count(Possui_ÃÂrgÃÂ£o_Gestor) # conta considerando o filtro
org_gestor

# Quantidade de entes federados com adesao publicada e que instituiram Conselho de politica cultural

conselho <- adesao %>%
  select(UF, Ente, SituaÃÂ§ÃÂ£o, Possui_Conselho) %>%
  filter(SituaÃÂ§ÃÂ£o == "Publicado no DOU" & Possui_Conselho == "Sim") %>% # filtra observando as duas condicoes
  count(Possui_Conselho) # conta considerando o filtro
conselho

# Quantidade de entes federados com adesao publicada e que definiram mecanismo para fomento da cultura

mec_fomento <- adesao %>%
  select(UF, Ente, SituaÃÂ§ÃÂ£o, Possui_Mecanismo_Fomento) %>%
  filter(SituaÃÂ§ÃÂ£o == "Publicado no DOU" & Possui_Mecanismo_Fomento == "Sim") %>% # filtra observando as duas condicoes
  count(Possui_Mecanismo_Fomento) # conta considerando o filtro
mec_fomento

# Quantidade de entes federados com ades?o publicada e que elaboraram plano de politica cultural

plano <- adesao %>%
  select(UF, Ente, SituaÃÂ§ÃÂ£o, Possui_Plano) %>%
  filter(SituaÃÂ§ÃÂ£o == "Publicado no DOU" & Possui_Plano == "Sim") %>% # filtra observando as duas condicoes
  count(Possui_Plano) # conta considerando o filtro
plano

# Quantidade de municipios por estado com adesao publicada e todos os componentes definidos

adesao_compl <- adesao %>%
  select(UF:Possui_Plano, -Codigo) %>%
  mutate(SituaÃÂ§ÃÂ£o == "Publicado no DOU", Possui_Sistema == "Sim", Possui_ÃÂrgÃÂ£o_Gestor == "Sim", 
         Possui_Conselho == "Sim", Possui_Mecanismo_Fomento == "Sim", Possui_Plano == "Sim") %>% # seleciona os municipios com todos os componentes
  group_by(UF) %>% # agrupa por estado
  count(UF) # conta municipios pelo grupo
glimpse(adesao_compl)
adesao_compl

# Grafico 2: quantidade de municipios por estado com todos os componentes do sistema definidos

ggplot(adesao_compl) +
  geom_bar(aes(x= UF, y = n, color = n, fill = n), stat = "identity") +
  labs(title="Municipios com todos os componentes do SNC", x="estados", y="numero de municipios") +
  coord_flip()

# Quantidade de municipios com todos os componentes definidos, em sua regiao

adesao_regiao <- adesao_compl %>%
  mutate(Regiao = 
           ifelse(UF %in% c("AC", "RO", "RR", "AM", "PA", "TO", "AP"), "Norte", 
                  ifelse(UF %in% c("MA", "PI", "CE", "RN", "PE", "PB", "SE", "AL", "BA"), "Nordeste",
                         ifelse(UF %in% c("MT", "MS", "GO", "DF"), "Centro-Oeste", 
                                ifelse(UF %in% c("SP", "RJ", "ES", "MG"), "Sudeste", "Sul"))))) # cria coluna com a regiao que o estado pertence
adesao_regiao

# Grafico 3: Numero de municipios nos estados com todos os componentes definidos, em sua regiao

ggplot(adesao_regiao) +
  geom_bar(aes(x= UF, y = n, color=Regiao, fill=Regiao), stat = "identity") +
  labs(title="Municipios com todos os componentes do sistema, por regiao", 
       x = "estados por regiao", y = "numero  de municipios") +
  coord_flip()

# Grafico 4: Numero de municipios nos estado, por regiao, com todos os componentes definidos

ggplot(adesao_regiao, aes(x = Regiao,  weights = n, stat = "identity")) +
  geom_bar(aes(fill = UF), color="Black") +
  geom_text(aes(x = Regiao, y = n, group = UF, label = UF),
            position = position_stack(vjust = 0.5), size=2) +
  guides(fill=FALSE) +
  xlab("Regiao do Brasil") + ylab("Numero de municipios") +
  labs(title="Municipios com todos os componentes, por regiao")

# Percentual de municipios nos estados com todos os componentes do sistema de cultura local definidos

adesao_regiao_agrupada <- adesao_regiao %>%
  group_by(Regiao) %>% # agrupa por regi?o
  summarise(contagem = sum(n)) # soma pelo grupo
adesao_regiao_agrupada

total_agrup <- adesao_regiao_agrupada %>%
  select(Regiao, contagem) %>%
  mutate(total_mun = c(467, 1794, 450, 1668, 1191)) # insere coluna contendo o numero total de municipios por regiao (IBGE)
total_agrup

total_mun_percent <- total_agrup %>%
  select(Regiao, contagem, total_mun) %>%
  group_by(Regiao) %>% # agrupa por regiao
  mutate(percentual = contagem / total_mun * 100) # calcula e cria coluna com o percentual de municipios por regiao
total_mun_percent

## Conclusoes
# Conforme arquivo index.rmd
