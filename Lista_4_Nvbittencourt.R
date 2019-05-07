#### Questao 1 ####
# link do Github: https://github.com/Nvbittencourt/Lista_4_Nathalia_Bittencourt 

install.packages("tidyverse")
install.packages("readxl")
install.packages("dplyr")
library(GGally)
library(ffbase)
library(readr)
library(rlang)
getwd()


#### Questao 2 ####

#An√°lise de correla√ß√£o entre o n√∫mero de alunos por docente e o IDH do munic√≠pio. Dados do PNUD 2010

#Base de dados no PNUD 

require(readxl)
require(ffbase)
setwd("C:/Users/nathi/Desktop/Mestrado/An√°lise Dados")
load("matricula_pe_censo_escolar_2016.RData")
load("docentes_pe_censo_escolar_2016.RData")
load("turmas_pe_censo_escolar_2016.RData")
load("escolas_pe_censo_escolar_2016.RData")

#abrindo dados referentes ao PNUD 2010
getwd()

# transformando a base de dados
pnud <- read_excel("atlas2013_dadosbrutos_pt.xlsx", sheet = 2)

# conhecendo a base:
head(pnud)
unique(pnud$ANO)

# selecionando apenas os dados referentes ao ano de 2010 e ao Estado de Pernambuco
pnud_pe_2010 <- pnud %>% filter(ANO == 2010 & UF == 26)

rm(pnud)  # removendo base pnud

# processamento da base de dados

# base TURMAS
require(tidyverse)

turmas_pe_sel <- turmas_pe %>% group_by(CO_MUNICIPIO) %>% summarise(n_turmas = n(), 
turmas_disc_prof = sum(IN_DISC_PROFISSIONALIZANTE, na.rm = T), turmas_disc_inf = sum(IN_DISC_INFORMATICA_COMPUTACAO, na.rm = T), turmas_disc_mat = sum(IN_DISC_MATEMATICA, na.rm = T), turmas_disc_pt = sum(IN_DISC_LINGUA_PORTUGUESA, na.rm = T), turmas_disc_en = sum(IN_DISC_LINGUA_INGLES, na.rm = T))

# verificacao

dim(turmas_pe_sel)[1] == length(unique(turmas_pe$CO_MUNICIPIO))

summary(turmas_pe_sel)

# base de Escolas

escolas_pe_sel <- escolas_pe %>% group_by(CO_MUNICIPIO) %>%
  summarise(n_escolas = n(), 
            n_escolas_priv = sum(TP_DEPENDENCIA == 4, na.rm = T),
            escolas_func = sum(TP_SITUACAO_FUNCIONAMENTO == 1, na.rm = T),
            escolas_agua_inex = sum(IN_AGUA_INEXISTENTE, na.rm = T),
            escolas_energia_inex = sum(IN_ENERGIA_INEXISTENTE, na.rm = T),
            escolas_esgoto_inex = sum(IN_ESGOTO_INEXISTENTE, na.rm = T),
            escolas_internet = sum(IN_INTERNET, na.rm = T),
            escolas_alimentacao = sum(IN_ALIMENTACAO, na.rm = T))

# verificacao

dim(escolas_pe_sel)[1] == length(unique(escolas_pe$CO_MUNICIPIO))

summary(escolas_pe_sel)

library(tidyverse)

# Docentes
install.packages(summ)
docentes_pe_sel <- docentes_pe %>% group_by(CO_MUNICIPIO) %>% summarise(n_docentes = n(), docentes_media_idade = mean(NU_IDADE), docentes_fem_sx = sum(TP_SEXO == 2, na.rm = T), docentes_superior = sum(TP_ESCOLARIDADE == 4, na.rm = T), docentes_contrato = sum(TP_TIPO_CONTRATACAO %in% c(1, 4), na.rm = T))

summary(docentes_pe_sel)

# Matriculas

matriculas_pe_sel <- matricula_pe %>% group_by(CO_MUNICIPIO) %>% summarise(n_matriculas = n(), alunos_media_idade = mean(NU_IDADE), alunos_fem_sx = sum(TP_SEXO == 2, na.rm = T), alunos_negros = sum(TP_COR_RACA %in% c(2, 3), na.rm = T), alunos_indigenas = sum(TP_COR_RACA == 5, na.rm = T), alunos_cor_nd = sum(TP_COR_RACA == 0, na.rm = T), matriculas_educ_inf = sum(TP_ETAPA_ENSINO %in% c(1, 2), na.rm = T), matriculas_educ_fund = sum(TP_ETAPA_ENSINO %in% c(4:21, 41), na.rm = T), matriculas_educ_medio = sum(TP_ETAPA_ENSINO %in% c(25:38), na.rm = T))

# verificacao

dim(matriculas_pe_sel)[1] == length(unique(matricula_pe$CO_MUNICIPIO))

summary(matriculas_pe_sel)

## Unindo a base de dados do censo e pnud: 

# matriculas

censo_pnud_pe_sel <- pnud_pe_2010 %>% full_join(matriculas_pe_sel, 
                                                by = c("Codmun7" = "CO_MUNICIPIO"))
dim(pnud_pe_2010)

dim(matriculas_pe_sel)

dim(censo_pnud_pe_sel)

names(censo_pnud_pe_sel)

# escolas

censo_pnud_pe_sel <- censo_pnud_pe_sel %>% full_join(escolas_pe_sel, 
                                                     by = c("Codmun7" = "CO_MUNICIPIO"))

dim(escolas_pe_sel)

dim(censo_pnud_pe_sel)

names(censo_pnud_pe_sel)

# turmas

censo_pnud_pe_sel <- censo_pnud_pe_sel %>% full_join(turmas_pe_sel, 
                                                     by = c("Codmun7" = "CO_MUNICIPIO"))

dim(turmas_pe_sel)

dim(censo_pnud_pe_sel)

names(censo_pnud_pe_sel)

# docentes
censo_pnud_pe_sel <- censo_pnud_pe_sel %>% full_join(docentes_pe_sel, 
                                                     by = c("Codmun7" = "CO_MUNICIPIO"))


dim(docentes_pe_sel)

dim(censo_pnud_pe_sel)

names(censo_pnud_pe_sel)

# salvando a nova base de dados

getwd()
dir()

save(censo_pnud_pe_sel, file = "2016_censo_pnud_pe_sel.RData")

write.csv2(censo_pnud_pe_sel, file = "2016_censo_pnud_pe_sel.csv",
           row.names = F)

rm(list = ls())  # limpando area de trabalho

#### Abrindo o novo banco de dados ####

# observando  o caminho da base de dados:

getwd()

# abrindo a base de dados:

load("2016_censo_pnud_pe_sel.RData")

## verificando algumas caracter?sticas da base de dados:

dim(censo_pnud_pe_sel)

summary(censo_pnud_pe_sel)

head(censo_pnud_pe_sel)

#### Questao 2 ####

#Nao deve haver docente com mais de 70 anos ou com menos de 18 anos:

summary(censo_pnud_pe_sel)

summary(censo_pnud_pe_sel$alunos_media_idade)

setwd("C:/Users/nathi/Desktop/Mestrado/An√°lise Dados")

load("docentes_pe_censo_escolar_2016.RData")

docentes_pe_selecao <- docentes_pe%>% filter(NU_IDADE > 18, NU_IDADE < 70)

dim(docentes_pe_selecao)


# Nao deve haver aluno com mais de 25 anos ou com menos de 1 ano #

load("matricula_pe_censo_escolar_2016.RData")

matricula_pe_selecao <- matricula_pe%>% filter(NU_IDADE > 1, NU_IDADE < 25)

dim(matricula_pe_selecao)

summary(matricula_pe_selecao$NU_IDADE)


# Apresente estat√≠sticas descritivas do numero de alunos por docente nos munic√≠pios do Estado:

dim(censo_pnud_pe_sel)

names(censo_pnud_pe_sel)

DocentesAlunos <- censo_pnud_pe_sel$n_matriculas/censo_pnud_pe_sel$n_docentes




# Estatistica Descritiva de Docentes por alunos:

DocentesAlunos

summary(DocentesAlunos)

plot(DocentesAlunos)

ggplot(censo_pnud_pe_sel, aes(DocentesAlunos))+geom_histogram()


# Apresente o municipio com maior numero de alunos por docente e seu IDHM

names(censo_pnud_pe_sel)

summary(DocentesAlunos)

DocentesAlunos

# juncao das variaveis

censo_pnud_pe_sel_docentesalunos <- censo_pnud_pe_sel %>%  mutate(DocentesAlunos)

View(censo_pnud_pe_sel_docentesalunos)

censo_pnud_pe_sel_docentesalunos["177", ]

# A cidade e Tupanatinga! 


# Faca o teste do coeficiente de correlacao linear de pearson e apresente sua resposta

cor(censo_pnud_pe_sel_docentesalunos$DocentesAlunos, censo_pnud_pe_sel_docentesalunos$IDHM)

cor.test(censo_pnud_pe_sel_docentesalunos$DocentesAlunos, censo_pnud_pe_sel_docentesalunos$IDHM)

#De acordo com os dados, a correlaÁ„o È "-0,47", isto È, negativa, o que indica que n„o h· correlaÁ„o entre o n∫ de discentes por aluno e o IDH dos municÌpios.


####Questao 3 ####

# Criando grafico de R de dispersao no ggplot

ggplot(censo_pnud_pe_sel_docentesalunos, aes(DocentesAlunos, IDHM, color = IDHM))+geom_point()
