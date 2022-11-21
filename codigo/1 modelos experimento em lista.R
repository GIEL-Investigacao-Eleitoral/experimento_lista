#-----------------------------------------------------
#                    Carregar
#-----------------------------------------------------
library(dplyr)
library(haven)
load("banco/BD Covid-Faperj.sav")
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

sum(is.na(banco$Q14_1))
sum(is.na(banco$Q14_2))
sum(is.na(banco$Q14_3))

table(experi)
banco %>% group_by(experimento) %>% summarise(media=mean(Q14_3))

banco$experimento2 = ifelse(is.na(banco$Q14_2),FALSE,TRUE)
banco$experimento3 = ifelse(banco$experimento2==T,1,0)
table(banco$experimento)

banco %>% group_by(experimento2) %>% summarise(media=mean(Q14_3))
banco %>% group_by(experimento3) %>% summarise(media=mean(Q14_3))

# Conduct test with null hypothesis that there is no design effect
teste <- ict.test(banco$Q14_3, banco$experimento2, J = 4, gms = TRUE)
print(teste)

banco$Q14_3 <- as.numeric(banco$Q14_3)

banco_reduzido = banco %>% select(Q14_3, experimento3)
banco_reduzido = na.omit(banco_reduzido)
# Tem que ser um data.frame para funcionar
# Não pode ser um tibble nem um spss
banco_reduzido = data.frame(banco_reduzido)
colnames(banco_reduzido) = c('y','treat')

# Calculate list experiment difference in means
diff_medias <- ictreg(y ~ 1, data = banco_reduzido, 
                     treat = "treat", J=4, method = "lm")
summary(diff_medias)

aaa <- predict(diff_medias,newdata = as.data.frame(matrix(1, 1, 1)), se.fit = TRUE, 
                                avg = TRUE)
aaa

