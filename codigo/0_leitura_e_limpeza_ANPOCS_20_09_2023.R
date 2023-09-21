#EXPERIMENTO EM LISTA

# library(dplyr)
# library(haven)
# banco <- haven::read_spss("C:/Users/08451589707/Documents/GitHub/experimento_lista/anpocs/BD Covid-Faperj.sav")
# 
# # criacao do dicionario de dados
# nomes<-names(banco)
# nomes <- data.frame(nomes)
# rotulos_perguntas<-lapply(banco, function(x) attributes(x)$label)
# rotulos_perguntas <- data.frame(matrix(unlist(rotulos_perguntas), nrow=length(rotulos_perguntas), byrow=F))
# rotulos_perguntas<-rotulos_perguntas[,1]
# dicionario<-data.frame(nomes,rotulos_perguntas)
# remove(nomes,rotulos_perguntas)
# names(dicionario) = c('variavel','descricao')
# 
# save(banco,dicionario,file = "C:/Users/08451589707/Documents/GitHub/experimento_lista/anpocs/BD_Covid_Faperj_Anpocs.RData")


#-----------------------------------------------------
#                    Carregar
#-----------------------------------------------------
library(dplyr)
library(haven)
load("C:/Users/08451589707/Documents/GitHub/experimento_lista/anpocs/BD_Covid_Faperj_Anpocs.RData")
dicionario = dicionario[,1]
#-----------------------------------------------------
#                    Limpeza
#-----------------------------------------------------

#------------------------------------------------------------------------------
#           Variavel resposta
#------------------------------------------------------------------------------

max(banco$Q14_1, na.rm = T)
max(banco$Q14_2, na.rm = T)

mean(banco$Q14_1, na.rm = T)
mean(banco$Q14_2, na.rm = T)
#mean(banco$Q14_3, na.rm = T)

sum(is.na(banco$Q14_1))
sum(is.na(banco$Q14_2))
sum(is.na(banco$Q14_3))

table(banco$experimento)


# Sociais
haven::print_labels(banco$sexo)
haven::print_labels(banco$faixaidade2)
haven::print_labels(banco$Raça2)
haven::print_labels(banco$escolaridade2)
haven::print_labels(banco$renda2)
haven::print_labels(banco$religião2)
haven::print_labels(banco$região3)

# Politicas
haven::print_labels(banco$Q5)
haven::print_labels(banco$Q6)
haven::print_labels(banco$Q7)
haven::print_labels(banco$Q8)
ideologia2
