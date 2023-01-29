
#-----------------------------------------------------
#                    Carregar
#-----------------------------------------------------
library(dplyr)
library(haven)
load("/home/steven/Área de Trabalho/CCJP/Borba/experimento_lista/BD_Covid_Faperj.RData")
load("C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/CCJP/Borba/02 novembro de 2022 experimento de lista/banco/BD_Covid_Faperj.RData")
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
library(flextable)
banco %>% tabyl(Q14_3,experimento) %>%
  adorn_percentages("col") %>% round(2) %>% flextable()


#----------------------------------------------------------------

# preparando o banco para o pacote 'list'
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

#----------------------------------------------------------------------
# incluindo as covariaveis
# sexo, idade, escolaridade, confianca na urna e ideologia
#----------------------------------------------------------------------
banco = tibble(banco)
banco$Q28 = as.factor(banco$Q28)
banco$Q28 = ifelse(banco$Q28=='99',NA,banco$Q28)
banco$Q28 = ifelse(banco$Q28=='6',NA,banco$Q28)
banco$Q28 = ifelse(banco$Q28=='1','Esquerda',banco$Q28)
banco$Q28 = ifelse(banco$Q28=='2','Centro-esquerda',banco$Q28)
banco$Q28 = ifelse(banco$Q28=='3','Centro',banco$Q28)
banco$Q28 = ifelse(banco$Q28=='4','Centro-direita',banco$Q28)
banco$Q28 = ifelse(banco$Q28=='5','Direita',banco$Q28)
table(banco$Q28)

banco$v5 = as.factor(banco$v5)
banco$v5 = ifelse(banco$v5=='1','1 Até o fundamental completo',banco$v5)
banco$v5 = ifelse(banco$v5=='2','1 Até o fundamental completo',banco$v5)
banco$v5 = ifelse(banco$v5=='3','1 Até o fundamental completo',banco$v5)
banco$v5 = ifelse(banco$v5=='4','1 Até o fundamental completo',banco$v5)
banco$v5 = ifelse(banco$v5=='5','2 Ensino médio completo',banco$v5)
banco$v5 = ifelse(banco$v5=='6','3 Superior completo',banco$v5)
banco$v5 = ifelse(banco$v5=='7','3 Superior completo',banco$v5)

banco$Q8 = as.factor(banco$Q8)
banco$Q8 = ifelse(banco$Q8=='1','1.Muita confiança',banco$Q8)
banco$Q8 = ifelse(banco$Q8=='2','2.Pouca confiança',banco$Q8)
banco$Q8 = ifelse(banco$Q8=='3','3.Nenhuma confiança',banco$Q8)
banco$Q8 = ifelse(banco$Q8=='4',NA,banco$Q8)
table(banco$Q8)

banco$Q5 = as.factor(banco$Q5)
banco$Q5 = ifelse(banco$Q5=='1','1.Muito importante',banco$Q5)
banco$Q5 = ifelse(banco$Q5=='2','2.Pouco importante',banco$Q5)
banco$Q5 = ifelse(banco$Q5=='3','3.Nada importante',banco$Q5)
banco$Q5 = ifelse(banco$Q5=='4',NA,banco$Q5)
table(banco$Q5)



banco_reduzido = banco %>% select(Q14_3, experimento3,Idade_Exata,v2,Q28,v5,Q8,Q5)
banco_reduzido = na.omit(banco_reduzido)
# Tem que ser um data.frame para funcionar
# Não pode ser um tibble nem um spss
banco_reduzido = data.frame(banco_reduzido)
colnames(banco_reduzido) = c('y','treat','idade','sexo','ideologia','educa','confia_urna','importância_voto')
colnames(banco_reduzido) 
# Calculate list experiment difference in means
diff_medias <- ictreg(y ~ 1, data = banco_reduzido, 
                     treat = "treat", J=4, method = "lm")
summary(diff_medias)

aaa <- predict(diff_medias, newdata = as.data.frame(matrix(1, 1, 1)), se.fit = TRUE, 
                                avg = TRUE)
aaa

#----------------------------------------------------------------------------------------
# Fit linear regression
modelo_simples <- ictreg(y ~ idade + as.factor(sexo), data = banco_reduzido, 
                     treat = "treat", J=4, method = "lm")
summary(modelo_simples)

modelo_intermediario1 <- ictreg(y ~ idade + as.factor(sexo)+educa, data = banco_reduzido, 
                     treat = "treat", J=4, method = "lm")
summary(modelo_intermediario1)

modelo_intermediario2 <- ictreg(y ~ idade + as.factor(sexo)+educa+ideologia, data = banco_reduzido, 
                     treat = "treat", J=4, method = "lm")
summary(modelo_intermediario2)

lm.results <- ictreg(y ~ idade + as.factor(sexo)+ideologia+educa+confia_urna+importância_voto, data = banco_reduzido, 
                     treat = "treat", J=4, method = "lm")
summary(lm.results)





#a estatística de teste para um teste Z tem a distribuição normal padrão sob a hipótese nula. 
#Suponha que você execute um teste Z de duas caudas com um α de 0,05,
#e obtenha uma estatística Z com base em seus dados de 2,5. 
#Este valor Z corresponde a um valor-p de 0,0124. 
#2*pnorm(2.5,lower.tail = FALSE)

#Este valor é 2 vezes a probabilidade de que o teste estatístico não assume um valor igual a ou maior que o valor absoluto do referido valor realmente observado com base na sua amostra (sob H0). 
#2* P(TS > |1,785|) = 2 * 0,0371 = 0,0742. Por isso o valor de p = 0,0742. 
#2*pnorm(1.785,lower.tail = FALSE) # (1-pnorm(1.785))*2

# 2*Pr(ET>|−2.14/1.08|) = 2*Pr(ET>|-1.981481|)= 2*Pr(ET>1.981481)

#ET = 2.14/1.08
#2*pnorm(ET,lower.tail = FALSE)
#2*pt(ET, df=20, lower.tail = FALSE, log.p = FALSE)


table(banco$experimento2)
banco_reduzido %>% group_by(treat,importância_voto) %>% summarise(media=mean(y))
banco_reduzido %>% group_by(treat,educa) %>% summarise(media=mean(y))
banco_reduzido %>% group_by(treat,idade) %>% summarise(media=mean(y))











# Fit two-step non-linear least squares regression
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
