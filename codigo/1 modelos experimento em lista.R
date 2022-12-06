
#-----------------------------------------------------
#                    Carregar
#-----------------------------------------------------
library(dplyr)
library(haven)
load("/home/steven/Área de Trabalho/CCJP/Borba/experimento_lista/BD_Covid_Faperj.RData")
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

#define standard error of mean function
erro_padrao <- function(x) sd(x)/sqrt(length(x))
tabela_medias = banco %>% group_by(experimento) %>% summarise(media=mean(Q14_3),erro_padrao=erro_padrao(Q14_3))
tabela_medias = as.data.frame(tibble(tabela_medias))
tabela_medias$experimento <- haven::as_factor(tabela_medias$experimento)

# http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
library(ggplot2)
p<- ggplot(tabela_medias, aes(x=experimento, y=media, color=experimento)) + 
  geom_pointrange(aes(ymin=media-1.96*erro_padrao, ymax=media+1.96*erro_padrao))
# Finished line plot
p+labs(x="Grupo", y = "Número médio de itens")+
  theme_classic() +
  scale_color_manual(values=c('#999999','#E69F00'))

# banco %>% group_by(experimento) %>% table(banco$Q14_3)
library(janitor)
banco %>% tabyl(Q14_3,experimento) %>%
  adorn_percentages("col") %>% round(2) %>% flextable()


banco$experimento2 = ifelse(is.na(banco$Q14_2),FALSE,TRUE)
banco$experimento3 = ifelse(banco$experimento2==T,1,0)
banco %>% group_by(experimento2) %>% summarise(media=mean(Q14_3))
banco %>% group_by(experimento3) %>% summarise(media=mean(Q14_3))

table(banco$experimento)

#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
# list = Statistical Methods for the Item Count Technique and List Experiment
library(list)

# Conduct test with null hypothesis that there is no design effect
teste <- ict.test(banco$Q14_3, banco$experimento2, J = 4, gms = TRUE)
print(teste)

teste2 <- ict.test(banco$Q14_3, banco$experimento2, J = 4, gms = FALSE)
print(teste2)

#gms	= A logical value indicating whether the generalized moment selection procedure should be used.

teste <- ict.test(banco$Q14_3, banco$experimento2, J = 4, gms = TRUE,pi.table	=TRUE,n.draws=50000)
print(teste)


banco$Q14_3 <- as.numeric(banco$Q14_3)

banco_reduzido = banco %>% select(Q14_3, experimento3,Idade_Exata,v2)
banco_reduzido = na.omit(banco_reduzido)
# Tem que ser um data.frame para funcionar
# Não pode ser um tibble nem um spss
banco_reduzido = data.frame(banco_reduzido)
colnames(banco_reduzido) = c('y','treat','idade','sexo')

# Calculate list experiment difference in means
diff_medias <- ictreg(y ~ 1, data = banco_reduzido, 
                     treat = "treat", J=4, method = "lm")
summary(diff_medias)

aaa <- predict(diff_medias, newdata = as.data.frame(matrix(1, 1, 1)), se.fit = TRUE, 
                                avg = TRUE)
aaa

#----------------------------------------------------------------------------------------
# Fit linear regression
lm.results <- ictreg(y ~ idade + as.factor(sexo), data = banco_reduzido, 
                     treat = "treat", J=4, method = "lm")
summary(lm.results)
# Fit two-step non-linear least squares regression
# Replicates Table 1 Columns 3-4 Imai (2011); note that age is divided by 10
nls.results <- ictreg(y ~ idade + as.factor(sexo), data = banco_reduzido, 
                      treat = "treat", J=4, method = "nls")
summary(nls.results)

# Fit EM algorithm ML model with constraint
ml.constrained.results <- ictreg(y ~ idade , data = banco_reduzido, 
                                 treat = "treat", J=4, method = "ml", 
                                 overdispersed = FALSE, constrained = TRUE)
summary(ml.constrained.results)
# Fit EM algorithm ML model with no constraint
ml.unconstrained.results <- ictreg(y ~ idade , data = banco_reduzido, 
                                   treat = "treat", J=4, method = "ml", 
                                   overdispersed = FALSE, constrained = FALSE)
summary(ml.unconstrained.results)
# Fit standard design ML model
noboundary.results <- ictreg(y ~ idade, data = banco_reduzido, treat = "treat",
                             J = 4, method = "ml", 
                             overdispersed = FALSE)

summary(noboundary.results)

# Robust models, which constrain sensitive item proportion
#   to difference-in-means estimate
robust.ml <- ictreg(
  y ~ idade + as.factor(sexo), data = banco_reduzido, treat = "treat",
  J = 4, method = "ml", robust = TRUE)
summary(robust.ml)

robust.nls <- ictreg(
  y ~ idade + as.factor(sexo), data = banco_reduzido, treat = "treat",
  J = 4, method = "nls", robust = TRUE)
summary(robust.nls)
