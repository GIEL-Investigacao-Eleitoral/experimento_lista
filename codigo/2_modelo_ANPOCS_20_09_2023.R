#-----------------------------------------------------
#                    Carregar
#-----------------------------------------------------
library(dplyr)
library(haven)
load("C:/Users/08451589707/Documents/GitHub/experimento_lista/anpocs/BD_Covid_Faperj_Anpocs.RData")
remove(dicionario)

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


banco = banco %>% filter(ideologia2!=99)
banco = banco %>% filter(Q8!=99)
banco = banco %>% filter(Q7!=99)
banco = banco %>% filter(Q5!=99)


#--------------------------------------------------------------------------------------

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
teste <- ict.test(banco$Q14_3, banco$experimento2, J = 4, gms = TRUE)
print(teste)

teste2 <- ict.test(banco$Q14_3, banco$experimento2, J = 4, gms = FALSE)
print(teste2)

#gms	= A logical value indicating whether the generalized moment selection procedure should be used.
teste <- ict.test(banco$Q14_3, banco$experimento2, J = 4, gms = TRUE,pi.table	=TRUE,n.draws=50000)
print(teste)

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
ideologia2

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
      renda2==1     ~ "Até 02",
      renda2==2     ~ "02 a 05",
      renda2==3     ~ "05 a 10",
      renda2==4     ~ "Mais de 10"))
table(banco_reduzido$renda2)


banco_reduzido = banco_reduzido %>%
  mutate(
    religião2 = case_when(
      religião2==1     ~ "Católica",
      religião2==2     ~ "Evangélica",
      religião2==3     ~ "Outras religiões",
      religião2==4     ~ "Sem religião"))
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
      Q6==1     ~ "A favor",
      Q6==2     ~ "Contra",
      Q6==3     ~ "Indiferente"))
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



















#banco_reduzido = na.omit(banco_reduzido)
# Tem que ser um data.frame para funcionar
# Não pode ser um tibble nem um spss
banco_reduzido = data.frame(banco_reduzido)
# Calculate list experiment difference in means
diff_medias <- ictreg(Q14_3 ~ 1, data = banco_reduzido, 
                      treat = "experimento2", J=4, method = "lm")
summary(diff_medias)

aaa <- predict(diff_medias, newdata = as.data.frame(matrix(1, 1, 1)), se.fit = TRUE, 
               avg = TRUE)
aaa

#----------------------------------------------------------------------------------------
# Fit linear regression
modelo_social <- ictreg(Q14_3 ~ sexo + faixaidade2 +Raça2+
                         escolaridade2+renda2+religião2+região3,
                         data = banco_reduzido, 
                         treat = "experimento2", J=4, method = "lm")
summary(modelo_social)

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




# Fit two-step non-linear least squares regression
nls_results <- ictreg(Q14_3 ~ sexo + faixaidade2 +Raça2+
                        escolaridade2+renda2+religião2+região3,
                      data = banco_reduzido, 
                      treat = "experimento2", J=4, method = "nls")
summary(nls_results)


# Fit EM algorithm ML model with constraint
ml.constrained.results <- ictreg(Q14_3 ~ sexo + faixaidade2 +Raça2+
                         escolaridade2+renda2+religião2+região3,
                         data = banco_reduzido, 
                                 overdispersed = FALSE, constrained = TRUE)
summary(ml.constrained.results)
# Fit EM algorithm ML model with no constraint
ml.unconstrained.results <- ictreg(y ~ idade + as.factor(sexo)+educa+confia_urna+importância_voto+regiao+renda, data = banco_reduzido,  
                                   treat = "treat", J=4, method = "ml", 
                                   overdispersed = FALSE, constrained = FALSE)
summary(ml.unconstrained.results)

# Fit standard design ML model
noboundary.results <- ictreg(y ~ idade + as.factor(sexo)+educa+confia_urna+importância_voto+regiao+renda, data = banco_reduzido,   
                             treat = "treat",J = 4, method = "ml", 
                             overdispersed = FALSE)

summary(noboundary.results)

# Robust models, which constrain sensitive item proportion
#   to difference-in-means estimate
robust.ml <- ictreg(
  y ~ idade + as.factor(sexo)+educa+confia_urna+importância_voto+regiao+renda, data = banco_reduzido,  
  treat = "treat",
  J = 4, method = "ml", robust = TRUE)
summary(robust.ml)

robust.nls <- ictreg(
  y ~ idade + as.factor(sexo)+educa+confia_urna+importância_voto+regiao+renda, data = banco_reduzido,  
  treat = "treat",
  J = 4, method = "nls", robust = TRUE)
summary(robust.nls)


