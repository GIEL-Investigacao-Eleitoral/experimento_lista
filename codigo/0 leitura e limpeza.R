#EXPERIMENTO EM LISTA

library(dplyr)
library(haven)
banco <- haven::read_spss("/home/steven/Área de Trabalho/CCJP/Borba/experimento_lista/BD Covid-Faperj(1).sav")

# criacao do dicionario de dados
nomes<-names(banco)
nomes <- data.frame(nomes)
rotulos_perguntas<-lapply(banco, function(x) attributes(x)$label)
rotulos_perguntas <- data.frame(matrix(unlist(rotulos_perguntas), nrow=length(rotulos_perguntas), byrow=F))
rotulos_perguntas<-rotulos_perguntas[,1]
dicionario<-data.frame(nomes,rotulos_perguntas)
remove(nomes,rotulos_perguntas)
names(dicionario) = c('variavel','descricao')

save(banco,dicionario,file = "/home/steven/Área de Trabalho/CCJP/Borba/experimento_lista/BD_Covid_Faperj.RData")


#-----------------------------------------------------
#                    Carregar
#-----------------------------------------------------
library(dplyr)
library(haven)
load("/home/steven/Área de Trabalho/CCJP/Borba/experimento_lista/BD_Covid_Faperj.RData")

#-----------------------------------------------------
#                    Limpeza
#-----------------------------------------------------

#------------------------------------------------------------------------------
#           Variavel resposta
#------------------------------------------------------------------------------
# a lista 1 tem 4 elementos
# a lista 2 tem 5 elementos
# a ultima variavel eh a combinacao linear das duas primeiras
dicionario[31,] # lista 1
dicionario[32,] # lista 2
dicionario[33,] # lista 1 + lista 2

max(banco$Q14_1, na.rm = T)
max(banco$Q14_2, na.rm = T)

mean(banco$Q14_1, na.rm = T)
mean(banco$Q14_2, na.rm = T)
#mean(banco$Q14_3, na.rm = T)

sum(is.na(banco$Q14_1))
sum(is.na(banco$Q14_2))
sum(is.na(banco$Q14_3))






haven::print_labels(banco$FILTRO_PROFISSÃO)

haven::print_labels(banco$FILIAÇÃO)

haven::print_labels(banco$Q8)
haven::print_labels(banco$v5)
haven::print_labels(banco$Q28)

#1   Muita confiança
#2   Pouca confiança
#3 Nenhuma confiança
#99          Não sabe

#Q8  - confiança na urna 
#v5 educa
#Q28 - ideologia
#value           label
#1        Esquerda
#2 Centro-esquerda
#3          Centro
#4  Centro-direita
#5         Direita
#96         Nenhuma
#99        Não sabe

haven::print_labels(banco$Q5)


haven::print_labels(banco$sexo)












































#------------------------------------------------------------------------------
#           Variavel resposta
#------------------------------------------------------------------------------
# Temos duas abordagens para criar a variavel resposta
# As duas permitem criar uma analise de sensibilidade
# 1 - Classificação1T = Classificação do candidato no final do primeiro turno
#     1=Eleito
#     2=Primeiro, mas vai pro 2º Turno
#     3=Segundo, vai pro 2º Turno
#     4=Segundo, não vai pro 2º Turno
# 
# 2 - Usar Votação percentual dos candidatos (votos totais)
# acima ou abaixo da mediana TV$VotaçãoPercent<=median(TV$VotaçãoPercent)
#------------------------------------------------------------------------

# #Classificação1T = Classificação do candidato no final do primeiro turno
# table(TV$Classificação1T)
# print_labels(TV$Classificação1T)
# barplot(table(TV$Classificação1T),col="navy") # variavel Classificação1T
# 
# #1=Eleito
# #2=Primeiro, mas vai pro 2º Turno
# #3=Segundo, vai pro 2º Turno
# #4=Segundo, não vai pro 2º Turno
# 
# TV$ida_ao_Segundo_turno <-ifelse(TV$Classificação1T==1 | TV$Classificação1T==2 | TV$Classificação1T==3,1,0)
# table(TV$Classificação1T,TV$ida_ao_Segundo_turno)
# 
# summary(TV$VotaçãoPercent)
# hist(TV$VotaçãoPercent,col="red")
# median(TV$VotaçãoPercent)
# sum(is.na(TV$VotaçãoPercent))
# 
# #TV$acima_da_mediana<- ifelse(TV$VotaçãoPercent<=median(TV$VotaçãoPercent),0,1)
# #TV$acima_10_perc<- if_else(TV$VotaçãoPercent<=10,0,1)
# 
# table(TV$ida_ao_Segundo_turno)
# 
# #------------------------------------------------------------------------------
# # Principais variaveis
# #------------------------------------------------------------------------------
# print_labels(TV$Ano)
# table(TV$Ano)
# TV$Ano2<- if_else(TV$Ano==10,1,0)
# table(TV$Ano2)
# 
# summary(TV$hgpe) # Tempo dos candidatos em segundos
# summary(TV$TempoPercent) # NÂO USAR 545%? -	Percentual de tempo dos candidatos 
# print_labels(TV$TipoCandidato)
# 
# library(reactable)
# TV %>%
#   group_by(ida_ao_Segundo_turno) %>%
#   #  group_by(acima_da_mediana) %>%
#   #  group_by(acima_10_perc) %>%
#   summarise(mean = mean(hgpe),  n = n()) %>% data.frame() %>% reactable()
# 
# 
# summary(TV$hgpe)
# TV$hgpe_minutos <- (TV$hgpe/60)
# summary(TV$hgpe_minutos)
# 
# #------------------------------------------------------------------------------
# # preditor linear
# print_labels(TV$Região)
# table(TV$Estado)
# 
# # Não usar!
# # PCdoB = PC DO B  = PC do B
# TV %>% pull(Partido) %>% table() %>% data.frame()  %>% View() 
# table(TV$Partido)
# #------------------------------------------------------------------------------
# #library(genderBR)
# #TV$sexo<-get_gender(TV$Candidato)
# table(TV$Sexo)
# table(TV$Sexo,TV$ida_ao_Segundo_turno)
# 
# 
# #-----------------------------------------------------------------------------------
# print_labels(TV$TipoCandidato)
# table(TV$TipoCandidato,TV$ida_ao_Segundo_turno)
# round(prop.table(table(TV$TipoCandidato,TV$ida_ao_Segundo_turno),1)*100,2)
# 
# Reeleição de Titular, Reeleição de Substituto,Governista,Oposição
# 
# TV$TipoCandidato <- ifelse(TV$TipoCandidato==2,1,TV$TipoCandidato)
# table(TV$TipoCandidato,TV$ida_ao_Segundo_turno)
# TV$TipoCandidato <- ifelse(TV$TipoCandidato==1,'Reeleição de Titular/Substituto',TV$TipoCandidato)
# TV$TipoCandidato <- ifelse(TV$TipoCandidato==3,'Governista',TV$TipoCandidato)
# TV$TipoCandidato <- ifelse(TV$TipoCandidato==4,'Oposição',TV$TipoCandidato)
# TV$TipoCandidato<-factor(TV$TipoCandidato,levels = c("Oposição","Governista","Reeleição de Titular/Substituto"))
# table(TV$TipoCandidato,TV$ida_ao_Segundo_turno)
# #-----------------------------------------------------------------------------------
# #-----------------------------------------------------------------------------------
# #-----------------------------------------------------------------------------------
# #-----------------------------------------------------------------------------------
# #-----------------------------------------------------------------------------------
# #-----------------------------------------------------------------------------------
# #-----------------------------------------------------------------------------------
# 
# names(TV)
# teste <-TV %>% group_by(Ano,Estado) %>% count() 
# TV <- TV %>% left_join(teste)
# TV <- TV %>% rename('numero_candidatos'='n')
# remove(teste)
# 
# TV$Idade <- as.numeric(TV$Idade)
# summary(TV$Idade)
# sum(is.na(TV$Idade))
# #summary(TV$Escolaridade) # tem que ordenar as variáveis
# table(TV$Escolaridade) # tem que ordenar as variáveis
# TV<-TV %>%
#   mutate(
#     educa = case_when(
#       Escolaridade=='Fundamental incompleto' ~ "Fundamental",
#       Escolaridade=='Fundamental completo'   ~ "Fundamental",
#       Escolaridade=='Médio incompleto'       ~ "Fundamental",
#       Escolaridade=='Médio completo'         ~ "Médio",
#       Escolaridade=='Superior incompleto'    ~ "Médio",
#       Escolaridade=='Superior completo'      ~ "Superior",
#       TRUE                    ~ "Não informado2"))
# table(TV$Escolaridade,TV$educa) 
# TV$educa<-ifelse(TV$educa=='Não informado2',NA,TV$educa)
# table(TV$educa) 
# 
# 
# summary(TV$Eleitorado)
# TV$Eleitorado  <-TV$Eleitorado/100000
# summary(TV$Eleitorado)
# 
# TV %>% select(Eleitorado,ida_ao_Segundo_turno) %>%
#   group_by(ida_ao_Segundo_turno) %>%
#   summarise(media=mean(Eleitorado))
