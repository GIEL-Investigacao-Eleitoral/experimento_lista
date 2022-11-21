#-------------------------------------------------
# OBS - ainda está com codigo antigo TEM QUE ATUALIZAR
#-------------------------------------------------

#-------------------------------------------------
# dois modelos:
#1. machine learning e
#2. regressao logistica

#-------------------------------------------------
#           Treino e Teste
#-------------------------------------------------
## 80% of the sample size
set.seed(12345)
smp_size <- floor(0.8 * nrow(TV))
train_ind <- sample(seq_len(nrow(TV)), size = smp_size)

treino <- TV[train_ind, ]
teste  <- TV[-train_ind, ]

remove(train_ind,smp_size)

# pratical guide to logistic regression
#-------------------------------------------------
#    Build Logit Models and Predict
#-------------------------------------------------
# hgpe_minutos
modelo0 <- glm(as.factor(ida_ao_Segundo_turno) ~ Sexo+educa+Idade+as.factor(Ano2)+hgpe_minutos+as.factor(TipoCandidato)+as.factor(Região), data=TV, family=binomial(link="logit"))
modelo1 <- glm(as.factor(ida_ao_Segundo_turno) ~ Sexo+educa+Idade+as.factor(Ano2)+hgpe_minutos+as.factor(TipoCandidato)+as.factor(Região)+as.factor(Ano2)*hgpe_minutos, data=TV, family=binomial(link="logit"))
modelo2 <- glm(as.factor(ida_ao_Segundo_turno) ~ Sexo+educa+Idade+numero_candidatos+as.factor(Ano2)+hgpe_minutos+as.factor(TipoCandidato)+as.factor(Região), data=TV, family=binomial(link="logit"))
modelo3 <- glm(as.factor(ida_ao_Segundo_turno) ~ Sexo+educa+Idade+numero_candidatos+as.factor(Ano2)+hgpe_minutos+as.factor(TipoCandidato)+as.factor(Região)+as.factor(Ano2)*hgpe_minutos, data=TV, family=binomial(link="logit"))

AIC(modelo0)
AIC(modelo1)
AIC(modelo2)
AIC(modelo3)
#How to disable scientific notation
options(scipen=999)
library(broom)
coeficientes0<-tidy(modelo0)
razaodechances0<-tidy(exp(modelo0$coefficients))
colnames(razaodechances0)[2]<-"razaodechances"
coef_rc0<-coeficientes0 %>% left_join(razaodechances0, by = c("term" = "names"))

coeficientes1<-tidy(modelo1)
razaodechances1<-tidy(exp(modelo1$coefficients))
colnames(razaodechances1)[2]<-"razaodechances"
coef_rc1<-coeficientes1 %>% left_join(razaodechances1, by = c("term" = "names"))

coeficientes2<-tidy(modelo2)
razaodechances2<-tidy(exp(modelo2$coefficients))
colnames(razaodechances2)[2]<-"razaodechances"
coef_rc2<-coeficientes2 %>% left_join(razaodechances2, by = c("term" = "names"))

coeficientes3<-tidy(modelo3)
razaodechances3<-tidy(exp(modelo3$coefficients))
colnames(razaodechances3)[2]<-"razaodechances"
coef_rc3<-coeficientes3 %>% left_join(razaodechances3, by = c("term" = "names"))

resultado<-list(coef_rc0,coef_rc1)
writexl::write_xlsx(resultado,path = "C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/CCJP/Borba/Tempo_TV_21_05_2021/resultados/result_broom_13_07_2021_v1.xlsx")

library(GGally)
g0<-ggcoef(modelo0, exponentiate = TRUE)
g1<-ggcoef(modelo1, exponentiate = TRUE)
g2<-ggcoef(modelo2, exponentiate = TRUE)
g3<-ggcoef(modelo3, exponentiate = TRUE)
ggsave("C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/CCJP/Borba/Tempo_TV_21_05_2021/resultados/grafico0_versao_10_07_2021.png", plot=g0, width = 40, height = 10, units = "cm")
ggsave("C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/CCJP/Borba/Tempo_TV_21_05_2021/resultados/grafico1_versao_10_07_2021.png", plot=g1, width = 40, height = 10, units = "cm")
ggsave("C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/CCJP/Borba/Tempo_TV_21_05_2021/resultados/grafico2_versao_10_07_2021.png", plot=g2, width = 40, height = 10, units = "cm")
ggsave("C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/CCJP/Borba/Tempo_TV_21_05_2021/resultados/grafico3_versao_10_07_2021.png", plot=g3, width = 40, height = 10, units = "cm")

#A generic method for calculating variable importance for objects produced by train and method specific methods
library(caret)
importancia_modelo0 <-caret::varImp(modelo0)
importancia_modelo1 <-caret::varImp(modelo1)
importancia_modelo2 <-caret::varImp(modelo2)
importancia_modelo3 <-caret::varImp(modelo3)

importancia <- list(importancia_modelo0,importancia_modelo1,importancia_modelo2,importancia_modelo3)
writexl::write_xlsx(importancia,path = "C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/CCJP/Borba/Tempo_TV_21_05_2021/resultados/importancia_variaveis_10_07_2021_v1.xlsx")

predicted <- predict(modelo1, teste, type="response")  # predicted scores

library(car)
vif(modelo0)
vif(modelo1)
vif(modelo2)
vif(modelo3)

#optimalCutoff não funciona com NA (missings)
teste$predicted<-predicted
table(teste$ida_ao_Segundo_turno,teste$predicted)
summary(teste$ida_ao_Segundo_turno)
summary(teste$predicted)
sum(is.na(teste$predicted))

library(tidyr)
teste2<-teste %>% drop_na(predicted)

library(InformationValue)
optCutOff <- optimalCutoff(actuals =teste2$ida_ao_Segundo_turno, 
                           predictedScores =teste2$predicted,
                           returnDiagnostics=TRUE)
optCutOff$optimalCutoff
optCutOff <- optCutOff[["optimalCutoff"]]
optCutOff

#Misclassification Error
#Misclassification error is the percentage mismatch of predcited vs actuals, irrespective of 1’s or 0’s. The lower the misclassification error, the better is your model.
#Mis-Classification Error is the proportion of all events that were incorrectly classified, for a given probability cutoff score.
misClassError(teste2$ida_ao_Segundo_turno, teste2$predicted, threshold = optCutOff)

#ROC
#Receiver Operating Characteristics Curve traces the percentage of true positives accurately predicted by a given logit model as the prediction probability cutoff is lowered from 1 to 0. For a good model, as the cutoff is lowered, it should mark more of actual 1’s as positives and lesser of actual 0’s as 1’s.

#An excellent model has AUC near to the 1 which means it has a good measure of separability. A poor model has AUC near to the 0 which means it has the worst measure of separability. In fact, it means it is reciprocating the result. It is predicting 0s as 1s and 1s as 0s. And when AUC is 0.5, it means the model has no class separation capacity whatsoever.
#A perfect predictor gives an AUC-ROC score of 1, a predictor which makes random guesses has an AUC-ROC score of 0.5.
#https://stats.stackexchange.com/questions/266387/can-auc-roc-be-between-0-0-5

plotROC(teste2$ida_ao_Segundo_turno, teste2$predicted)

#Concordance is the percentage of predicted probability scores where the scores of actual positive’s are greater than the scores of actual negative’s
Concordance(teste2$ida_ao_Segundo_turno, teste2$predicted)

#Specificity and Sensitivity
#Sensitivity (or True Positive Rate) is the percentage of 1’s (actuals) correctly predicted by the model, while, specificity is the percentage of 0’s (actuals) correctly predicted. Specificity can also be calculated as 1 − False Positive Rate.
#Sensitivity=# Actual 1′s and Predicted as 1′s# of Actual 1′s
#Specificity=# Actual 0′s and Predicted as 0′s# of Actual 0′s

sensitivity(teste2$ida_ao_Segundo_turno, teste2$predicted, threshold = optCutOff)
specificity(teste2$ida_ao_Segundo_turno, teste2$predicted, threshold = optCutOff)

#The above numbers are calculated on the validation sample that was not used for training the model. 
#So, a truth detection rate of 58% on test data is good.

teste2$predicted2<-ifelse(teste2$predicted<optCutOff,0,
                          ifelse(teste2$predicted>optCutOff,1,NA))
teste2$predicted2

table(teste2$ida_ao_Segundo_turno, teste2$predicted2)
confusionMatrix(teste2$ida_ao_Segundo_turno,teste2$predicted2, threshold = optCutOff)


#----------------------------------------------------------------------
#------------------------------------------------------------------------------
# O MODELO de MACHINE LEARNING
#------------------------------------------------------------------------------
#----------------------------------------------------------------------
library(tidyverse)
library(caret)
library(glmnet)

#------------------------------------------------------------------------------
# O MODELO GAM
#------------------------------------------------------------------------------
#Note that, many concepts for linear regression hold true for the logistic regression modeling. For example, you need to perform some diagnostics (Chapter @ref(logistic-regression-assumptions-and-diagnostics)) to make sure that the assumptions made by the model are met for your data.
library("mgcv")
# Fit the model
modelo_gam <- gam(as.factor(ida_ao_Segundo_turno) ~ Sexo+educa+Idade+Eleitorado+numero_candidatos+as.factor(Ano2)+hgpe_minutos+as.factor(TipoCandidato)+as.factor(Região), data=TV, family=binomial(link="logit"))
# Summarize model
summary(modelo_gam)
# Make predictions
predicted_gam <- modelo_gam %>% predict(teste, type = "response")


