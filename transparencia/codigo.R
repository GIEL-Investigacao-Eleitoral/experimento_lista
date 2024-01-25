#-----------------------------------------------------
#                    Carregar
#-----------------------------------------------------

library(dplyr)
library(haven)
library(list)

banco = readRDS('C:/Users/Documents/GitHub/experimento_lista/transparencia/banco.rds')

#----------------------------------------------------------------------------------------
#Calculate list experiment difference in means
diff_medias <- ictreg(Q14_3 ~ 1, data = banco, 
                      treat = "experimento2", J=4, method = "lm")
summary(diff_medias)

aaa <- predict(diff_medias, newdata = as.data.frame(matrix(1, 1, 1)), se.fit = TRUE, 
               avg = TRUE)
aaa
remove(aaa)

#----------------------------------------------------------------------------------------
# Fit linear regression
modelo_social <- ictreg(Q14_3 ~ sexo + faixaidade2 +Raça2+
                          escolaridade2+renda2+religião2+região3,
                        data = banco, 
                        treat = "experimento2", J=4, method = "lm")
summary(modelo_social)

# Fit two-step non-linear least squares regression
nls_modelo_social<- ictreg(Q14_3 ~ sexo + faixaidade2 +Raça2+
                             escolaridade2+renda2+religião2+região3,
                           data = banco, 
                           treat = "experimento2", J=4, method = "nls")
summary(nls_modelo_social)


# Fit EM algorithm ML model with constraint
ml_modelo_social <- ictreg(Q14_3 ~ sexo + faixaidade2 +Raça2+
                             escolaridade2+renda2+religião2+região3,
                           data = banco, 
                           treat = "experimento2", J=4,
                           overdispersed = FALSE, constrained = TRUE)
summary(ml_modelo_social)
# Fit EM algorithm ML model with no constraint
ml_modelo_social2 <- ictreg(Q14_3 ~ sexo + faixaidade2 +Raça2+
                              escolaridade2+renda2+religião2+região3,
                            data = banco, 
                            treat = "experimento2", J=4, method = "ml", 
                            overdispersed = FALSE, constrained = FALSE)
summary(ml_modelo_social)

# Fit standard design ML model
ml_modelo_social3 <- ictreg(Q14_3 ~ sexo + faixaidade2 +Raça2+
                              escolaridade2+renda2+religião2+região3,
                            data = banco, 
                            treat = "experimento2", J=4, method = "ml", 
                            overdispersed = FALSE)

summary(ml_modelo_social3)

# Robust models, which constrain sensitive item proportion
#   to difference-in-means estimate
robust_modelo_social <- ictreg(
  Q14_3 ~ sexo + faixaidade2 +Raça2+
    escolaridade2+renda2+religião2+região3,
  data = banco, 
  treat = "experimento2", J=4, method = "ml", robust = TRUE)
summary(robust_modelo_social)

modelo_social2_robusto <- ictreg(
  Q14_3 ~ sexo + faixaidade2 +Raça2+
    escolaridade2+renda2+religião2+região3,
  data = banco, 
  treat = "experimento2",
  J = 4, method = "nls", robust = TRUE)
summary(modelo_social2_robusto)


#--------------------------------------------------------------------------------
# Modelo com as variáveis políticas
#--------------------------------------------------------------------------------

modelo_pol <- ictreg(Q14_3 ~ ideologia2 + Q5 + Q6 + Q7+ Q8,
                     data = banco, 
                     treat = "experimento2", J=4, method = "lm")
summary(modelo_pol)

robust_modelo_pol2 <- ictreg(
  Q14_3 ~ ideologia2 + Q5 + Q7+ Q8,
  data = banco, 
  treat = "experimento2",
  J = 4, method = "nls", robust = TRUE)
summary(robust_modelo_pol2)

#--------------------------------------------------------------------------------
# Modelo completo
#--------------------------------------------------------------------------------

modelo_completo<- ictreg(Q14_3 ~ sexo + faixaidade2 +Raça2+ escolaridade2+renda2+religião2+região3 + ideologia2 + Q5  + Q6 + Q7+ Q8,
                         data = banco, treat = "experimento2", J=4, method = "lm")
summary(modelo_completo)

