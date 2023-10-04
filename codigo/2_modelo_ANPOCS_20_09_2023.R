#-----------------------------------------------------
#                    Carregar
#-----------------------------------------------------

library(dplyr)
library(haven)
load("C:/Users/08451589707/Documents/GitHub/experimento_lista/anpocs/BD_Covid_Faperj_Anpocs.RData")
remove(dicionario)

banco = banco %>% filter(ideologia2!=99)
banco = banco %>% filter(Q8!=99)
banco = banco %>% filter(Q7!=99)
banco = banco %>% filter(Q6!=96)
banco = banco %>% filter(Q5!=99)



#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
# list = Statistical Methods for the Item Count Technique and List Experiment
library(list)
# preparando o banco para o pacote 'list'
banco$experimento2 = ifelse(is.na(banco$Q14_2),FALSE,TRUE)
banco %>% group_by(experimento2) %>% summarise(media=mean(Q14_3))

table(banco$experimento)


# Conduct test with null hypothesis that there is no design effect
#teste <- ict.test(banco$Q14_3, banco$experimento2, J = 4, gms = TRUE)
#print(teste)
#teste2 <- ict.test(banco$Q14_3, banco$experimento2, J = 4, gms = FALSE)
#print(teste2)
#gms	= A logical value indicating whether the generalized moment selection procedure should be used.
#teste <- ict.test(banco$Q14_3, banco$experimento2, J = 4, gms = TRUE,pi.table	=TRUE,n.draws=50000)
#print(teste)

#----------------------------------------------------------------------
# incluindo as covariaveis
#----------------------------------------------------------------------
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
haven::print_labels(banco$ideologia2)

table(banco$Q6)

attributes(banco$Q6) 
attributes(banco$Q7) 
attributes(banco$Q8) 
  
banco_reduzido = banco %>% select(Q14_3, experimento2,
                                  # Sociais
                                  sexo,faixaidade2,Raça2,
                                  escolaridade2,renda2,religião2,região3,
                                  # Politicas
                                  Q5,Q6,Q7,Q8,ideologia2)


banco_reduzido$sexo = ifelse(banco_reduzido$sexo==1,'Masculino','Feminino')
table(banco_reduzido$sexo)
banco_reduzido = banco_reduzido %>%
  mutate(
    faixaidade2 = case_when(
      faixaidade2==1     ~ "16 a 24 anos",
      faixaidade2==2     ~ "25 a 34 anos",
      faixaidade2==3     ~ "35 a 44 anos",
      faixaidade2==4     ~ "45 a 59 anos",
      faixaidade2==5     ~ "60 anos ou mais"))
table(banco_reduzido$faixaidade2)
banco_reduzido = banco_reduzido %>%
  mutate(
    Raça2 = case_when(
      Raça2==1     ~ "Preta/Parda",
      Raça2==2     ~ "Branca",
      Raça2==3     ~ "Outras"))
table(banco_reduzido$Raça2)

banco_reduzido = banco_reduzido %>%
  mutate(
    escolaridade2 = case_when(
      escolaridade2==1     ~ "Até Ensino Fundamental",
      escolaridade2==2     ~ "Até Ensino Médio",
      escolaridade2==3     ~ "Ensino Superior"))
table(banco_reduzido$escolaridade2)

banco_reduzido = banco_reduzido %>%
  mutate(
    renda2 = case_when(
      renda2==1     ~ "1. Até 02",
      renda2==2     ~ "2. 02 a 05",
      renda2==3     ~ "3. 05 a 10",
      renda2==4     ~ "4. Mais de 10"))
table(banco_reduzido$renda2)

banco_reduzido = banco_reduzido %>%
  mutate(
    religião2 = case_when(
      religião2==1     ~ "4 Católica",
      religião2==2     ~ "1 Evangélica",
      religião2==3     ~ "2 Outras religiões",
      religião2==4     ~ "3 Sem religião"))
table(banco_reduzido$religião2)
banco_reduzido = banco_reduzido %>%
  mutate(
    região3 = case_when(
      região3==1     ~ "Capital",
      região3==2     ~ "Metropolitana",
      região3==3     ~ "Interior"))
table(banco_reduzido$região3)
#----------------------------------------------------------------------
banco_reduzido = banco_reduzido %>%
  mutate(
    Q5 = case_when(
      Q5==1     ~ "Muito importante",
      Q5==2     ~ "Pouco importante",
      Q5==3     ~ "Nada importante"))
table(banco_reduzido$Q5)

banco_reduzido = banco_reduzido %>%
  mutate(
    Q6 = case_when(
      Q6==1     ~ "2 A favor",
      Q6==2     ~ "1 Contra" )) #,
      # Q6==3     ~ "Indiferente"))
table(banco_reduzido$Q6)

banco_reduzido = banco_reduzido %>%
  mutate(
    Q7 = case_when(
      Q7==1     ~ "Sim",
      Q7==2     ~ "Não"))
table(banco_reduzido$Q7)

banco_reduzido = banco_reduzido %>%
  mutate(
    Q8 = case_when(
      Q8==1     ~ "Muita confiança",
      Q8==2     ~ "Pouca confiança",
      Q8==3     ~ "Nenhuma confiança"))
table(banco_reduzido$Q8)

banco_reduzido = banco_reduzido %>%
  mutate(
    ideologia2 = case_when(
      ideologia2==1     ~ "4 Esquerda",
      ideologia2==2     ~ "3 Centro",
      ideologia2==5     ~ "2 Direita",
      ideologia2==96     ~ "1 Nenhuma"))
table(banco_reduzido$ideologia2)

#banco_reduzido = na.omit(banco_reduzido)
# Tem que ser um data.frame para funcionar
# Não pode ser um tibble nem um spss
banco_reduzido = data.frame(banco_reduzido)


#----------------------------------------------------------------------------------------
#Calculate list experiment difference in means
diff_medias <- ictreg(Q14_3 ~ 1, data = banco_reduzido, 
                      treat = "experimento2", J=4, method = "lm")
summary(diff_medias)

aaa <- predict(diff_medias, newdata = as.data.frame(matrix(1, 1, 1)), se.fit = TRUE, 
               avg = TRUE)
aaa
remove(aaa,diff_medias,teste,teste2)

#----------------------------------------------------------------------------------------
# Fit linear regression
modelo_social <- ictreg(Q14_3 ~ sexo + faixaidade2 +Raça2+
                         escolaridade2+renda2+religião2+região3,
                         data = banco_reduzido, 
                         treat = "experimento2", J=4, method = "lm")
summary(modelo_social)

# Fit two-step non-linear least squares regression
nls_modelo_social<- ictreg(Q14_3 ~ sexo + faixaidade2 +Raça2+
                        escolaridade2+renda2+religião2+região3,
                      data = banco_reduzido, 
                      treat = "experimento2", J=4, method = "nls")
summary(nls_modelo_social)


# Fit EM algorithm ML model with constraint
ml_modelo_social <- ictreg(Q14_3 ~ sexo + faixaidade2 +Raça2+
                                   escolaridade2+renda2+religião2+região3,
                                 data = banco_reduzido, 
                                 treat = "experimento2", J=4,
                                 overdispersed = FALSE, constrained = TRUE)
summary(ml_modelo_social)
# Fit EM algorithm ML model with no constraint
ml_modelo_social2 <- ictreg(Q14_3 ~ sexo + faixaidade2 +Raça2+
                                     escolaridade2+renda2+religião2+região3,
                                   data = banco_reduzido, 
                                   treat = "experimento2", J=4, method = "ml", 
                                   overdispersed = FALSE, constrained = FALSE)
summary(ml_modelo_social)

# Fit standard design ML model
ml_modelo_social3 <- ictreg(Q14_3 ~ sexo + faixaidade2 +Raça2+
                               escolaridade2+renda2+religião2+região3,
                             data = banco_reduzido, 
                             treat = "experimento2", J=4, method = "ml", 
                             overdispersed = FALSE)

summary(ml_modelo_social3)

# Robust models, which constrain sensitive item proportion
#   to difference-in-means estimate
robust_modelo_social <- ictreg(
  Q14_3 ~ sexo + faixaidade2 +Raça2+
    escolaridade2+renda2+religião2+região3,
  data = banco_reduzido, 
  treat = "experimento2", J=4, method = "ml", robust = TRUE)
summary(robust_modelo_social)

modelo_social2_robusto <- ictreg(
  Q14_3 ~ sexo + faixaidade2 +Raça2+
    escolaridade2+renda2+religião2+região3,
  data = banco_reduzido, 
  treat = "experimento2",
  J = 4, method = "nls", robust = TRUE)
summary(modelo_social2_robusto)


#--------------------------------------------------------------------------------
# Modelo com as variáveis políticas
#--------------------------------------------------------------------------------

modelo_pol <- ictreg(Q14_3 ~ ideologia2 + Q5 + Q6 + Q7+ Q8,
                        data = banco_reduzido, 
                        treat = "experimento2", J=4, method = "lm")
summary(modelo_pol)

robust_modelo_pol2 <- ictreg(
  Q14_3 ~ ideologia2 + Q5 + Q7+ Q8,
  data = banco_reduzido, 
  treat = "experimento2",
  J = 4, method = "nls", robust = TRUE)
summary(robust_modelo_pol2)

#--------------------------------------------------------------------------------
# Modelo completo
#--------------------------------------------------------------------------------

modelo_completo<- ictreg(Q14_3 ~ sexo + faixaidade2 +Raça2+ escolaridade2+renda2+religião2+região3 + ideologia2 + Q5  + Q6 + Q7+ Q8,
                     data = banco_reduzido, treat = "experimento2", J=4, method = "lm")
summary(modelo_completo)

