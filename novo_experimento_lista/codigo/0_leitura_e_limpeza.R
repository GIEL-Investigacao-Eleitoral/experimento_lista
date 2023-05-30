#EXPERIMENTO EM LISTA
library(readxl)
library(dplyr)
library(janitor)
banco <- read_xlsx("C:/Users/Hp/Google Drive (steven.ross@uniriotec.br)/CCJP/Borba/29_05_2023/BDconcatenado2.xlsx") %>% clean_names()
names(banco)
table(banco$abaixo_ha_uma_lista_de_itens_com_criterios_que_os_eleitores_usam_para_escolher_o_seu_candidato_a_prefeito_a_com_quantos_desses_criterios_o_a_sr_a_concorda_nao_precisamos_saber_quais_estamos_interessados_apenas_na_quantidade_a_a_honestidade_e_o_preparo_para_o_cargo_do_candidato_b_as_propostas_de_governo_do_candidato_c_o_candidato_ser_a_indicacao_de_alguma_pessoa_de_confianca_do_eleitor_d_o_partido_politico_do_candidato_indique_abaixo_o_numero_de_itens_com_os_quais_o_a_sr_a_concorda_0_nenhum_1_2_3_ou_4)
table(banco$controle)

banco$resposta = banco$abaixo_ha_uma_lista_de_itens_com_criterios_que_os_eleitores_usam_para_escolher_o_seu_candidato_a_prefeito_a_com_quantos_desses_criterios_o_a_sr_a_concorda_nao_precisamos_saber_quais_estamos_interessados_apenas_na_quantidade_a_a_honestidade_e_o_preparo_para_o_cargo_do_candidato_b_as_propostas_de_governo_do_candidato_c_o_candidato_ser_a_indicacao_de_alguma_pessoa_de_confianca_do_eleitor_d_o_partido_politico_do_candidato_indique_abaixo_o_numero_de_itens_com_os_quais_o_a_sr_a_concorda_0_nenhum_1_2_3_ou_4
table(banco$controle,banco$resposta)

#------------------------------------------------------------------------------
#           Variavel resposta
#------------------------------------------------------------------------------
# a lista 1 tem 4 elementos
# a lista 2 tem 5 elementos
class(banco$resposta)
banco$resposta = as.numeric(banco$resposta)
banco$controle = as.factor(banco$controle)
banco = banco %>% filter(!is.na(resposta))


#define standard error of mean function
erro_padrao <- function(x) sd(x)/sqrt(length(x))
tabela_medias = banco %>% group_by(controle) %>% summarise(media=mean(resposta),erro_padrao=erro_padrao(resposta))

# http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
library(ggplot2)
p<- ggplot(tabela_medias, aes(x=controle, y=media, color=controle)) + 
  geom_pointrange(aes(ymin=media-1.96*erro_padrao, ymax=media+1.96*erro_padrao))
# Finished line plot
p+labs(x="Grupo", y = "Número médio de itens")+
  theme_classic() +
  scale_color_manual(values=c('#999999','#E69F00'))

# banco %>% group_by(experimento) %>% table(banco$Q14_3)
library(janitor)
library(flextable)
banco %>% tabyl(resposta,controle) %>%
  adorn_percentages("col") %>% round(2) %>% flextable()


#----------------------------------------------------------------

# preparando o banco para o pacote 'list'
banco$controle2 = ifelse(banco$controle=="0",FALSE,TRUE)
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
teste <- ict.test(banco$resposta, banco$controle2, J = 4, gms = TRUE)
print(teste)

teste2 <- ict.test(banco$resposta, banco$controle2, J = 4, gms = FALSE)
print(teste2)

#gms	= A logical value indicating whether the generalized moment selection procedure should be used.
teste <- ict.test(banco$resposta, banco$controle2, J = 4, gms = TRUE,pi.table	=TRUE,n.draws=50000)
print(teste)

#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------
#----------------------------------------------------------------------
# incluindo as covariaveis
# sexo, idade, escolaridade, confianca na urna e ideologia
#----------------------------------------------------------------------
banco = tibble(banco)
banco$Q28 = as.factor(banco$Q28)
banco$Q28 = ifelse(banco$Q28=='99',NA,banco$Q28)
banco$Q28 = ifelse(banco$Q28=='6',NA,banco$Q28)
banco$Q28 = ifelse(banco$Q28=='1','Esquerda',banco$Q28)
#banco$Q28 = ifelse(banco$Q28=='2','Centro-esquerda',banco$Q28)
banco$Q28 = ifelse(banco$Q28=='2','Centro',banco$Q28)
banco$Q28 = ifelse(banco$Q28=='3','Centro',banco$Q28)
#banco$Q28 = ifelse(banco$Q28=='4','Centro-direita',banco$Q28)
banco$Q28 = ifelse(banco$Q28=='4','Centro',banco$Q28)
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

table(banco$v7)
haven::print_labels(banco$v7)
banco$v7 = as.factor(banco$v7)
banco$v7 = ifelse(banco$v7=='1','Região Metropolitana',banco$v7)
banco$v7 = ifelse(banco$v7=='2','Região Metropolitana',banco$v7)
banco$v7 = ifelse(banco$v7=='3','Interior',banco$v7)
banco$v7 = ifelse(banco$v7=='4','Interior',banco$v7)
banco$v7 = ifelse(banco$v7=='5','Interior',banco$v7)
banco$v7 = ifelse(banco$v7=='6','Interior',banco$v7)
banco$v7 = ifelse(banco$v7=='7','Interior',banco$v7)
banco$v7 = ifelse(banco$v7=='8','Capital',banco$v7)
banco$v7 = ifelse(banco$v7=='9','Capital',banco$v7)
banco$v7 = ifelse(banco$v7=='10','Capital',banco$v7)
banco$v7 = ifelse(banco$v7=='11','Capital',banco$v7)
banco$v7 = ifelse(banco$v7=='12','Capital',banco$v7)
banco$v7 = ifelse(banco$v7=='13','Capital',banco$v7)
table(banco$v7)

table(banco$COTA_RENDA_FAIXA)
haven::print_labels(banco$COTA_RENDA_FAIXA)
banco$COTA_RENDA_FAIXA = as.factor(banco$COTA_RENDA_FAIXA)
banco$COTA_RENDA_FAIXA = ifelse(banco$COTA_RENDA_FAIXA=='1','Até 2 SM',banco$COTA_RENDA_FAIXA)
banco$COTA_RENDA_FAIXA = ifelse(banco$COTA_RENDA_FAIXA=='2','Entre 2 e 5 SM',banco$COTA_RENDA_FAIXA)
banco$COTA_RENDA_FAIXA = ifelse(banco$COTA_RENDA_FAIXA=='3','Mais de 5 SM',banco$COTA_RENDA_FAIXA)
banco$COTA_RENDA_FAIXA = ifelse(banco$COTA_RENDA_FAIXA=='4','Mais de 5 SM',banco$COTA_RENDA_FAIXA)


banco_reduzido = banco %>% select(Q14_3, experimento3,Idade_Exata,v2,Q28,v5,Q8,Q5,v7,COTA_RENDA_FAIXA)
banco_reduzido = na.omit(banco_reduzido)
# Tem que ser um data.frame para funcionar
# Não pode ser um tibble nem um spss
banco_reduzido = data.frame(banco_reduzido)
colnames(banco_reduzido) = c('y','treat','idade','sexo','ideologia','educa','confia_urna','importância_voto','regiao','renda')
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

lm.results <- ictreg(y ~ idade + as.factor(sexo)+ideologia+educa+confia_urna+importância_voto+regiao+renda, data = banco_reduzido, 
                     treat = "treat", J=4, method = "lm")
summary(lm.results)

ideologia
lm.results <- ictreg(y ~ idade + as.factor(sexo)+educa+confia_urna+importância_voto+regiao+renda, data = banco_reduzido, 
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









# basico , lm
lm.results <- ictreg(y ~ idade + as.factor(sexo)+educa+confia_urna+importância_voto+regiao+renda, data = banco_reduzido, 
                     treat = "treat", J=4, method = "lm")
summary(lm.results)

# Fit two-step non-linear least squares regression
nls_results <- ictreg(y ~ idade + as.factor(sexo)+educa+confia_urna+importância_voto+regiao+renda, data = banco_reduzido, 
                      treat = "treat", J=4, method = "nls")
summary(nls_results)


# Fit EM algorithm ML model with constraint
ml.constrained.results <- ictreg(y ~ idade + as.factor(sexo) + educa+confia_urna+importância_voto+regiao+renda, data = banco_reduzido,  
                                 treat = "treat", J=4, method = "ml", 
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
