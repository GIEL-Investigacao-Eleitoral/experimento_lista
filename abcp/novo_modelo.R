#Variáveis Sociais
#Sexo
#Idade
#Escolaridade
#Cor
#Região

#Variáveis Políticas
#Voto obrigatório
#Confiança na urna
#Confiança na justiça eleitoral
#Comprovante impresso do voto
#Ideologia (esquerda / centro-esquerda, centro, centro direita / direita)
#Vitima de violência política (sim, não)
#Eleito / não eleito
#Reeleição / não reeleição
#comparar vereador com prefeito/viceprefeito

#-----------------------------------------------------
#                    Carregar
#-----------------------------------------------------

library(dplyr)
library(list)
library(readxl)
library(janitor)
#BD <- read_excel("H:/Meu Drive/backup/dasktop/pasta_pessoal/Borba_Vinicius_experimento_lista/BDconcatenado2.xlsx") %>% clean_names()
BD <- read_excel("/home/steven/Downloads/Borba_Vinicius_experimento_lista/BDconcatenado2.xlsx") %>% clean_names()

head(BD)

# experimento
table(BD$abaixo_ha_uma_lista_de_itens_com_criterios_que_os_eleitores_usam_para_escolher_o_seu_candidato_a_prefeito_a_com_quantos_desses_criterios_o_a_sr_a_concorda_nao_precisamos_saber_quais_estamos_interessados_apenas_na_quantidade_a_a_honestidade_e_o_preparo_para_o_cargo_do_candidato_b_as_propostas_de_governo_do_candidato_c_o_candidato_ser_a_indicacao_de_alguma_pessoa_de_confianca_do_eleitor_d_o_partido_politico_do_candidato_indique_abaixo_o_numero_de_itens_com_os_quais_o_a_sr_a_concorda_0_nenhum_1_2_3_ou_4)
table(BD$abaixo_ha_uma_lista_de_itens_com_criterios_que_os_eleitores_usam_para_escolher_o_seu_candidato_a_prefeito_a_com_quantos_desses_criterios_o_a_sr_a_concorda_nao_precisamos_saber_quais_estamos_interessados_apenas_na_quantidade_a_a_honestidade_e_o_preparo_para_o_cargo_do_candidato_b_as_propostas_de_governo_do_candidato_c_o_candidato_ser_a_indicacao_de_alguma_pessoa_de_confianca_do_eleitor_d_o_partido_politico_do_candidato_indique_abaixo_o_numero_de_itens_com_os_quais_o_a_sr_a_concorda_0_nenhum_1_2_3_ou_4,BD$controle)
table(BD$controle)

class(BD$abaixo_ha_uma_lista_de_itens_com_criterios_que_os_eleitores_usam_para_escolher_o_seu_candidato_a_prefeito_a_com_quantos_desses_criterios_o_a_sr_a_concorda_nao_precisamos_saber_quais_estamos_interessados_apenas_na_quantidade_a_a_honestidade_e_o_preparo_para_o_cargo_do_candidato_b_as_propostas_de_governo_do_candidato_c_o_candidato_ser_a_indicacao_de_alguma_pessoa_de_confianca_do_eleitor_d_o_partido_politico_do_candidato_indique_abaixo_o_numero_de_itens_com_os_quais_o_a_sr_a_concorda_0_nenhum_1_2_3_ou_4)
BD %>%
  group_by(as.factor(controle)) %>%
  summarise(media = mean(as.numeric(abaixo_ha_uma_lista_de_itens_com_criterios_que_os_eleitores_usam_para_escolher_o_seu_candidato_a_prefeito_a_com_quantos_desses_criterios_o_a_sr_a_concorda_nao_precisamos_saber_quais_estamos_interessados_apenas_na_quantidade_a_a_honestidade_e_o_preparo_para_o_cargo_do_candidato_b_as_propostas_de_governo_do_candidato_c_o_candidato_ser_a_indicacao_de_alguma_pessoa_de_confianca_do_eleitor_d_o_partido_politico_do_candidato_indique_abaixo_o_numero_de_itens_com_os_quais_o_a_sr_a_concorda_0_nenhum_1_2_3_ou_4), na.rm=TRUE),
            dp = sd(as.numeric(abaixo_ha_uma_lista_de_itens_com_criterios_que_os_eleitores_usam_para_escolher_o_seu_candidato_a_prefeito_a_com_quantos_desses_criterios_o_a_sr_a_concorda_nao_precisamos_saber_quais_estamos_interessados_apenas_na_quantidade_a_a_honestidade_e_o_preparo_para_o_cargo_do_candidato_b_as_propostas_de_governo_do_candidato_c_o_candidato_ser_a_indicacao_de_alguma_pessoa_de_confianca_do_eleitor_d_o_partido_politico_do_candidato_indique_abaixo_o_numero_de_itens_com_os_quais_o_a_sr_a_concorda_0_nenhum_1_2_3_ou_4), na.rm=TRUE))


table(BD$nr_turno) # todos do primeiro turno
table(BD$ds_situacao_candidatura) # todos aptos
table(BD$sg_partido)

table(BD$ds_genero)
table(BD$nr_idade_data_posse)
table(BD$ds_grau_instrucao)
table(BD$ds_cor_raca)
table(BD$sg_uf)


table(BD$no_brasil_o_voto_e_obrigatorio_o_a_sr_a_e_a_favor_ou_contra_o_voto_obrigatorio)
table(BD$o_a_sr_a_diria_que_tem_muita_confianca_pouca_confianca_ou_nenhuma_confianca_na_justica_eleitoral)
table(BD$o_a_sr_a_diria_que_tem_muita_confianca_pouca_confianca_ou_nenhuma_confianca_na_urna_eletronica)
table(BD$o_a_sr_a_e_a_favor_ou_contra_o_comprovante_impresso_do_voto)
table(BD$na_politica_as_pessoas_falam_muito_de_esquerda_e_direita_onde_o_a_sr_a_situaria_a_ideologia_do_partido_pelo_qual_voce_concorreu_na_eleicao_municipal_de_2020)
table(BD$na_eleicao_municipal_de_2020_o_a_sr_a_foi_vitima_de_algum_tipo_de_violencia_por_causa_da_sua_atuacao_politica_como_candidato_a)
table(BD$st_reeleicao)
table(BD$ds_sit_totalizacao)
table(BD$ds_cargo)


BD = BD %>%
  mutate(
    regiao = case_when(
    sg_uf=="AC" ~ "Norte",
    sg_uf=="AL" ~ "Nordeste",
    sg_uf=="AM" ~ "Norte",
    sg_uf=="AP" ~ "Norte",
    sg_uf=="BA" ~ "Nordeste",
    sg_uf=="CE" ~ "Nordeste",
    sg_uf=="ES" ~ "Sudeste",
    sg_uf=="GO" ~ "Centro-Oeste",
    sg_uf=="MA" ~ "Nordeste",
    sg_uf=="MG" ~ "Sudeste",
    sg_uf=="MS" ~ "Centro-Oeste",
    sg_uf=="MT" ~ "Centro-Oeste",
    sg_uf=="PA" ~ "Norte",
    sg_uf=="PB" ~ "Nordeste",
    sg_uf=="PE" ~ "Nordeste",
    sg_uf=="PI" ~ "Nordeste",
    sg_uf=="PR" ~ "Sul",
    sg_uf=="RJ" ~ "Sudeste",
    sg_uf=="RN" ~ "Nordeste",
    sg_uf=="RO" ~ "Norte",
    sg_uf=="RR" ~ "Norte",
    sg_uf=="RS" ~ "Sul",
    sg_uf=="SC" ~ "Sul",
    sg_uf=="SE" ~ "Nordeste",
    sg_uf=="SP" ~ "Sudeste",
    sg_uf=="TO" ~ "Norte",
    TRUE                      ~ "outro"))

   
BD = BD %>%
  mutate(
    fidade = case_when(
      nr_idade_data_posse<=24 ~ '16 a 24 anos',
      nr_idade_data_posse<=34 ~ '25 a 34 anos',
      nr_idade_data_posse<=44 ~ '35 a 44 anos',
      nr_idade_data_posse<=59 ~ '45 a 59 anos',
      nr_idade_data_posse<=85 ~ '60 anos ou mais',
      TRUE                      ~ "other"))


BD = BD %>%
  mutate(
    educa = case_when(
      ds_grau_instrucao=='LÊ E ESCREVE' ~ 'Até o Ensino Fundamental',
      ds_grau_instrucao=='ENSINO FUNDAMENTAL INCOMPLETO' ~ 'Até o Ensino Fundamental',
      ds_grau_instrucao=='ENSINO FUNDAMENTAL COMPLETO' ~ 'Até o Ensino Fundamental',
      ds_grau_instrucao=="ENSINO MÉDIO INCOMPLETO" ~ 'Até o Ensino Médio',
      ds_grau_instrucao=="ENSINO MÉDIO COMPLETO" ~ 'Até o Ensino Médio',
      ds_grau_instrucao=="SUPERIOR INCOMPLETO" ~ 'Até o Ensino Superior',
      ds_grau_instrucao=="SUPERIOR COMPLETO" ~ 'Até o Ensino Superior',
      TRUE                      ~ "other"))




BD$cor_raca = ifelse(BD$ds_cor_raca=='BRANCA','BRANCA','NÃO BRANCA')


BD = BD %>%
  mutate(
    ideologia = case_when(
      na_politica_as_pessoas_falam_muito_de_esquerda_e_direita_onde_o_a_sr_a_situaria_a_ideologia_do_partido_pelo_qual_voce_concorreu_na_eleicao_municipal_de_2020=="Centro" ~ "Centro", 
      na_politica_as_pessoas_falam_muito_de_esquerda_e_direita_onde_o_a_sr_a_situaria_a_ideologia_do_partido_pelo_qual_voce_concorreu_na_eleicao_municipal_de_2020=="Centro-Direita" ~ "Direita", 
      na_politica_as_pessoas_falam_muito_de_esquerda_e_direita_onde_o_a_sr_a_situaria_a_ideologia_do_partido_pelo_qual_voce_concorreu_na_eleicao_municipal_de_2020=="Centro-Esquerda" ~ "Esquerda", 
      na_politica_as_pessoas_falam_muito_de_esquerda_e_direita_onde_o_a_sr_a_situaria_a_ideologia_do_partido_pelo_qual_voce_concorreu_na_eleicao_municipal_de_2020=="Direita" ~ "Direita", 
      na_politica_as_pessoas_falam_muito_de_esquerda_e_direita_onde_o_a_sr_a_situaria_a_ideologia_do_partido_pelo_qual_voce_concorreu_na_eleicao_municipal_de_2020=="Esquerda" ~ "Esquerda", 
      TRUE                      ~ NA))
      
#table(BD$ideologia)
#table(BD$na_sua_opiniao_as_eleicoes_para_prefeito_possuem_muita_fraude_pouca_fraude_ou_nenhuma_fraude)

BD$fraude = BD$na_sua_opiniao_as_eleicoes_para_prefeito_possuem_muita_fraude_pouca_fraude_ou_nenhuma_fraude
BD$fraude = factor(BD$fraude,levels = c("Não sei informar","Nenhuma fraude","Pouca fraude","Muita fraude"))
table(BD$fraude)

BD_reduzido = BD %>% select(ds_cargo,ds_sit_totalizacao,st_reeleicao,na_eleicao_municipal_de_2020_o_a_sr_a_foi_vitima_de_algum_tipo_de_violencia_por_causa_da_sua_atuacao_politica_como_candidato_a,na_politica_as_pessoas_falam_muito_de_esquerda_e_direita_onde_o_a_sr_a_situaria_a_ideologia_do_partido_pelo_qual_voce_concorreu_na_eleicao_municipal_de_2020,o_a_sr_a_e_a_favor_ou_contra_o_comprovante_impresso_do_voto,o_a_sr_a_diria_que_tem_muita_confianca_pouca_confianca_ou_nenhuma_confianca_na_urna_eletronica,o_a_sr_a_diria_que_tem_muita_confianca_pouca_confianca_ou_nenhuma_confianca_na_justica_eleitoral,no_brasil_o_voto_e_obrigatorio_o_a_sr_a_e_a_favor_ou_contra_o_voto_obrigatorio,sg_uf,ds_cor_raca,ds_grau_instrucao,nr_idade_data_posse,ds_genero, abaixo_ha_uma_lista_de_itens_com_criterios_que_os_eleitores_usam_para_escolher_o_seu_candidato_a_prefeito_a_com_quantos_desses_criterios_o_a_sr_a_concorda_nao_precisamos_saber_quais_estamos_interessados_apenas_na_quantidade_a_a_honestidade_e_o_preparo_para_o_cargo_do_candidato_b_as_propostas_de_governo_do_candidato_c_o_candidato_ser_a_indicacao_de_alguma_pessoa_de_confianca_do_eleitor_d_o_partido_politico_do_candidato_indique_abaixo_o_numero_de_itens_com_os_quais_o_a_sr_a_concorda_0_nenhum_1_2_3_ou_4,controle,regiao,fidade,cor_raca,ideologia,educa,fraude)

BD_reduzido = BD_reduzido %>% rename(resposta = abaixo_ha_uma_lista_de_itens_com_criterios_que_os_eleitores_usam_para_escolher_o_seu_candidato_a_prefeito_a_com_quantos_desses_criterios_o_a_sr_a_concorda_nao_precisamos_saber_quais_estamos_interessados_apenas_na_quantidade_a_a_honestidade_e_o_preparo_para_o_cargo_do_candidato_b_as_propostas_de_governo_do_candidato_c_o_candidato_ser_a_indicacao_de_alguma_pessoa_de_confianca_do_eleitor_d_o_partido_politico_do_candidato_indique_abaixo_o_numero_de_itens_com_os_quais_o_a_sr_a_concorda_0_nenhum_1_2_3_ou_4)
BD_reduzido = BD_reduzido %>% rename(idade = nr_idade_data_posse)
BD_reduzido = BD_reduzido %>% rename(voto_obrigatorio = no_brasil_o_voto_e_obrigatorio_o_a_sr_a_e_a_favor_ou_contra_o_voto_obrigatorio)
BD_reduzido = BD_reduzido %>% rename(confia_justica_eleitoral = o_a_sr_a_diria_que_tem_muita_confianca_pouca_confianca_ou_nenhuma_confianca_na_justica_eleitoral)
BD_reduzido = BD_reduzido %>% rename(confia_urna = o_a_sr_a_diria_que_tem_muita_confianca_pouca_confianca_ou_nenhuma_confianca_na_urna_eletronica)
BD_reduzido = BD_reduzido %>% rename(comprovante_impresso = o_a_sr_a_e_a_favor_ou_contra_o_comprovante_impresso_do_voto)
BD_reduzido = BD_reduzido %>% rename(vit_vio_eleitoral = na_eleicao_municipal_de_2020_o_a_sr_a_foi_vitima_de_algum_tipo_de_violencia_por_causa_da_sua_atuacao_politica_como_candidato_a)

BD_reduzido$resposta=gsub('NA',NA,BD_reduzido$resposta)

BD_reduzido$ds_cor_raca=gsub('SEM INFORMAÇÃO',NA,BD_reduzido$ds_cor_raca)

BD_reduzido$voto_obrigatorio=gsub('NA',NA,BD_reduzido$voto_obrigatorio)
BD_reduzido$voto_obrigatorio=gsub('Prefiro não opinar',NA,BD_reduzido$voto_obrigatorio)

BD_reduzido$confia_justica_eleitoral=gsub('NA',NA,BD_reduzido$confia_justica_eleitoral)
BD_reduzido$confia_justica_eleitoral=gsub('Não sei informar',NA,BD_reduzido$confia_justica_eleitoral)

BD_reduzido$confia_urna=gsub('NA',NA,BD_reduzido$confia_urna)
BD_reduzido$confia_urna=gsub('Não sei informar',NA,BD_reduzido$confia_urna)

BD_reduzido$comprovante_impresso=gsub('NA',NA,BD_reduzido$comprovante_impresso)
BD_reduzido$comprovante_impresso=gsub('Não sei informar',NA,BD_reduzido$comprovante_impresso)

BD_reduzido$ideologia=gsub('NA',NA,BD_reduzido$ideologia)
BD_reduzido$vit_vio_eleitoral=gsub('NA',NA,BD_reduzido$vit_vio_eleitoral)                          
BD_reduzido$vit_vio_eleitoral=gsub('Prefiro não informar',NA,BD_reduzido$vit_vio_eleitoral)        

BD_reduzido$ideologia=gsub('Não sei informar',NA,BD_reduzido$ideologia)
BD_reduzido$ideologia=gsub('Nenhuma delas',NA,BD_reduzido$ideologia)


BD_reduzido$idade = as.numeric(BD_reduzido$idade)
summary(BD_reduzido$idade)

BD_reduzido = na.omit(BD_reduzido)
class(BD_reduzido$resposta)

BD_reduzido$resposta = as.numeric(BD_reduzido$resposta)


BD_reduzido = BD_reduzido %>% filter(ds_cargo!="VICE-PREFEITO")
class(BD_reduzido)
BD_reduzido = data.frame(BD_reduzido)
#BD_reduzido_v = BD_reduzido %>% filter(ds_cargo=='VEREADOR')

#table(BD_reduzido$confia_urna)
#table(BD_reduzido$confia_justica_eleitoral)
#table(BD_reduzido$comprovante_impresso)

BD_reduzido$confia_urna = factor(BD_reduzido$confia_urna, levels = c("Nenhuma confiança", "Pouca confiança","Muita confiança"))
BD_reduzido$confia_justica_eleitoral = factor(BD_reduzido$confia_justica_eleitoral, levels = c("Nenhuma confiança", "Pouca confiança","Muita confiança"))
BD_reduzido$comprovante_impresso = factor(BD_reduzido$comprovante_impresso, levels = c("Indiferente","A favor","Contra"))

#----------------------------------------------------------------------------------------
#Calculate list experiment difference in means
diff_medias <- ictreg(resposta ~ 1, data = BD_reduzido, 
                      treat = "controle", J=4, method = "lm")
summary(diff_medias)
list::ict.test(BD_reduzido$resposta,BD_reduzido$controle, J=4)

aaa <- predict(diff_medias, newdata = as.data.frame(matrix(1, 1, 1)), se.fit = TRUE, avg = TRUE)
aaa
remove(aaa)

# MODELO COMPLETO
modelo <- ictreg(resposta ~ ds_genero+fidade+educa+cor_raca+regiao+
                   voto_obrigatorio+confia_justica_eleitoral+confia_urna+comprovante_impresso+ideologia+vit_vio_eleitoral+ds_sit_totalizacao+ds_cargo+fraude, 
                 data = BD_reduzido, 
                      treat = "controle", J=4, method = "lm")

resultado_modelo_completo = capture.output(summary(summary(modelo)))
cat(resultado_modelo_completo,file="resultado_modelo_completo.txt",sep="\n",append=FALSE)


summary(modelo)

# MODELO COM VARIÁVEIS SOCIAIS
modelo2 <- ictreg(resposta ~ ds_genero+fidade+educa+cor_raca+regiao, 
                 data = BD_reduzido, treat = "controle", J=4, method = "lm")

summary(modelo2)


# MODELO VARIÁVEIS POLITICAS
modelo3 <- ictreg(resposta ~ voto_obrigatorio+confia_justica_eleitoral+confia_urna+comprovante_impresso+ideologia+vit_vio_eleitoral+ds_sit_totalizacao+ds_cargo+fraude, 
                 data = BD_reduzido, 
                 treat = "controle", J=4, method = "lm")



summary(modelo3)


resultado_modelo3 = capture.output(summary(summary(modelo3)))
cat(resultado_modelo3,file="resultado_modelo3.txt",sep="\n",append=FALSE)

list::ict.test(BD_reduzido$resposta,BD_reduzido$controle,J=4,gms = TRUE)






#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
# Two-sample design
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
library(misty)
library(dplyr)

# Two-Sided 95% CI for y1 by group1
# unknown population variances, unequal variance assumption
# print results with 3 digits
resultado1 = ci.mean.diff(resposta ~ controle, data = BD_reduzido, digits = 3, na.omit = TRUE)

resultado1[["result"]]
resultado11 = data.frame(resultado1[["result"]][7],
                         resultado1[["result"]][8],
                         resultado1[["result"]][9])
resultado11 = resultado11[2,]
resultado11$categoria = '100 global'
resultado11

library(ggplot2)
#ggplot(resultado11, aes(x=1, y=m.diff, group=1)) +
#  geom_pointrange(aes(ymin = low, ymax = upp), color='#bfbfbf', fill='#bfbfbf',linewidth = 2)+
#  geom_point(shape=21, size=5, fill='#bfbfbf')+
#  geom_hline(yintercept = 0,linetype='dashed', col = 'red') + 
#  coord_flip()+
#  theme_classic()


# Two-Sided 95% CI for y1, y2, and y3 by group1
# unknown population variances, unequal variance assumption,
# analysis by group2 separately
resultado_sexo = ci.mean.diff(resposta ~ controle, data = BD_reduzido, digits = 3, na.omit = TRUE, group =BD_reduzido$ds_genero)
resultado_sexo1 = data.frame(resultado_sexo[["result"]][8],
                             resultado_sexo[["result"]][9],
                             resultado_sexo[["result"]][10])
resultado_sexo1 = resultado_sexo1[c(2,4),]
resultado_sexo1$categoria = c('101 feminino','102 masculino')
resultado = resultado11 %>% add_row(resultado_sexo1)
resultado$x = 1:3

# idade2
resultado_idade = ci.mean.diff(resposta ~ controle, data = BD_reduzido, digits = 3, na.omit = TRUE, group =BD_reduzido$fidade)
resultado_idade[["result"]]
resultado_idade = data.frame(resultado_idade[["result"]][8],
                             resultado_idade[["result"]][9],
                             resultado_idade[["result"]][10])
resultado_idade = resultado_idade[c(2,4,6,8,10),]
resultado_idade$categoria = c('104 16 a 24 anos','105 25 a 34 anos',
                              '106 35 a 44 anos','107 45 a 59 anos',
                              '108 60 anos ou mais')
resultado_idade
resultado_idade$x = 4:8
resultado = resultado %>% add_row(resultado_idade)

# Raça
resultado_raca = ci.mean.diff(resposta ~ controle, data = BD_reduzido, digits = 3, na.omit = TRUE, group =BD_reduzido$cor_raca)
resultado_raca[["result"]]
resultado_raca = data.frame(resultado_raca[["result"]][8],
                            resultado_raca[["result"]][9],
                            resultado_raca[["result"]][10])
resultado_raca = resultado_raca[c(2,4),]
resultado_raca$categoria = c('109 - Branca','110 - Não Branca')
resultado_raca
resultado_raca$x = 9:10
resultado = resultado %>% add_row(resultado_raca)
remove(resultado_idade,resultado_raca,resultado_sexo1,resultado11,resultado1,resultado_sexo)

# Educa

resultado_educa = ci.mean.diff(resposta ~ controle, data = BD_reduzido, digits = 3, na.omit = TRUE, group =BD_reduzido$educa)
resultado_educa[["result"]]
resultado_educa = data.frame(resultado_educa[["result"]][8],
                             resultado_educa[["result"]][9],
                             resultado_educa[["result"]][10])
resultado_educa = resultado_educa[c(2,4,6),]
resultado_educa$categoria = c('112 Até o Ensino Fundamental','113 Até o Ensino Médio',
                              '114 Até o Ensino Superior')
resultado_educa
resultado_educa$x = 12:14
resultado = resultado %>% add_row(resultado_educa)
remove(resultado_educa)

# região3

table(BD_reduzido$regiao)

resultado_região = ci.mean.diff(resposta ~ controle, data = BD_reduzido, digits = 3, na.omit = TRUE, group =BD_reduzido$regiao)
resultado_região[["result"]]
resultado_região = data.frame(resultado_região[["result"]][8],
                               resultado_região[["result"]][9],
                               resultado_região[["result"]][10])
resultado_região = resultado_região[c(2,4,6,8,10),]
resultado_região$categoria = c('123 Centro-Oeste','124 Nordeste',
                                '125 Norte','126 Sudeste','127 Sul')
resultado_região
resultado_região$x = 15:19
resultado = resultado %>% add_row(resultado_região)

remove(resultado_região)

#writexl::write_xlsx(resultado,path = "resultado_1.xlsx")

library(ggplot2)
ggplot(resultado, aes(x=categoria, y=m.diff, group=categoria,color=categoria,fill=categoria)) +
  geom_pointrange(aes(ymin = low, y=m.diff ,ymax = upp),linewidth = 2,shape=21, size=1.5)+
  geom_hline(yintercept = 0,linetype='dashed', col = 'black') +
  scale_colour_manual(values = c("red", "blue","blue","red", "red", "red","red", "red", "blue","blue","red","red", "red","blue","blue","blue","blue","blue")) +
  scale_fill_manual(values = c("red", "blue","blue","red", "red","red", "red","red", "blue","blue","red","red", "red","blue","blue","blue","blue","blue")) +
  
  #scale_colour_manual(values = c("#bfbfbf", "#3f3f3f","#3f3f3f","#bfbfbf", "#bfbfbf","#bfbfbf","#bfbfbf","#bfbfbf", "#3f3f3f","#3f3f3f","#3f3f3f","#bfbfbf", "#bfbfbf","#bfbfbf", "#3f3f3f", "#3f3f3f", "#3f3f3f", "#3f3f3f", "#bfbfbf", "#bfbfbf","#bfbfbf","#bfbfbf","#3f3f3f","#3f3f3f","#3f3f3f")) +
  #scale_fill_manual(values = c("#bfbfbf", "#3f3f3f","#3f3f3f","#bfbfbf", "#bfbfbf","#bfbfbf","#bfbfbf","#bfbfbf", "#3f3f3f","#3f3f3f","#3f3f3f","#bfbfbf", "#bfbfbf","#bfbfbf", "#3f3f3f", "#3f3f3f", "#3f3f3f", "#3f3f3f","#bfbfbf", "#bfbfbf","#bfbfbf","#bfbfbf","#3f3f3f","#3f3f3f","#3f3f3f")) +
  coord_flip()+
  theme_classic()+
  theme(legend.position="none")+
  labs(y='Diferença entre as médias',x='')
ggsave("C:/Users/08451589707/Documents/GitHub/experimento_lista/abcp/grafico1_sociais.png",
       width = 17,
       height = 17,
       units = "cm",dpi = 300)

#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------

resultado_geral = ci.mean.diff(resposta ~ controle, data = BD_reduzido, digits = 3, na.omit = TRUE)
resultado_geral = data.frame(resultado_geral[["result"]][7],
                             resultado_geral[["result"]][8],
                             resultado_geral[["result"]][9])
resultado_geral = resultado_geral[2,]
resultado_geral$categoria = '100            Global'
resultado_geral$x = 0
resultado_geral

#Variáveis Políticas
#Voto obrigatório
resultado_VO = ci.mean.diff(resposta ~ controle, data = BD_reduzido, digits = 3, na.omit = TRUE, group =BD_reduzido$voto_obrigatorio)
resultado_VO = data.frame(resultado_VO[["result"]][8],
                          resultado_VO[["result"]][9],
                          resultado_VO[["result"]][10])
resultado_VO = resultado_VO[c(2,4,6),]
resultado_VO$categoria = c('101           A favor','103            Contra','102       Indiferente')
resultado_VO$x = 1:3
resultado = resultado_geral %>% add_row(resultado_VO)
remove(resultado_VO,resultado_geral)


#Confiança na urna
resultado_Q7 = ci.mean.diff(resposta ~ controle, data = BD_reduzido, digits = 3, na.omit = TRUE, group =BD_reduzido$confia_urna)
resultado_Q7
resultado_Q7 = data.frame(resultado_Q7[["result"]][8],
                          resultado_Q7[["result"]][9],
                          resultado_Q7[["result"]][10])
resultado_Q7 = resultado_Q7[c(2,4,6),]
resultado_Q7$categoria = c('104   Muita confiança','106 Nenhuma confiança','105   Pouca confiança')
resultado_Q7$x = 4:6
resultado = resultado %>% add_row(resultado_Q7)
remove(resultado_Q7)



#Confiança na justiça eleitoral
resultado_Q8 = ci.mean.diff(resposta ~ controle, data = BD_reduzido, digits = 3, na.omit = TRUE, group =BD_reduzido$confia_justica_eleitoral)
resultado_Q8 = data.frame(resultado_Q8[["result"]][8],
                          resultado_Q8[["result"]][9],
                          resultado_Q8[["result"]][10])
resultado_Q8 = resultado_Q8[c(2,4,6),]
resultado_Q8$categoria = c('126   Muita confiança','128 Nenhuma confiança','127   Pouca confiança')
resultado_Q8$x = 7:9
resultado = resultado %>% add_row(resultado_Q8)
remove(resultado_Q8)

# table(BD_reduzido$resposta, BD_reduzido$confia_justica_eleitoral,BD_reduzido$controle)


# ideologia2
resultado_ideologia2 = ci.mean.diff(resposta ~ controle, data = BD_reduzido , digits = 3, na.omit = TRUE, group =BD_reduzido$ideologia)
resultado_ideologia2 = data.frame(resultado_ideologia2[["result"]][8],
                                  resultado_ideologia2[["result"]][9],
                                  resultado_ideologia2[["result"]][10])
resultado_ideologia2 = resultado_ideologia2[c(2,4,6),]
resultado_ideologia2$categoria = c('110            Centro','111           Direita','109          Esquerda')
resultado_ideologia2$x = 10:12
resultado = resultado %>% add_row(resultado_ideologia2)

remove(resultado_ideologia2)


#Comprovante impresso do voto

resultado_civ = ci.mean.diff(resposta ~ controle, data = BD_reduzido , digits = 3, na.omit = TRUE, group =BD_reduzido$comprovante_impresso)
resultado_civ = data.frame(resultado_civ[["result"]][8],
                           resultado_civ[["result"]][9],
                           resultado_civ[["result"]][10])
resultado_civ = resultado_civ[c(2,4,6),]
resultado_civ$categoria = c('131           A favor','133            Contra','132       Indiferente')
resultado_civ$x = 14:16
resultado = resultado %>% add_row(resultado_civ)

remove(resultado_civ)


#Vitima de violência política (sim, não)
resultado_vp = ci.mean.diff(resposta ~ controle, data = BD_reduzido , digits = 3, na.omit = TRUE, group =BD_reduzido$vit_vio_eleitoral)
resultado_vp = data.frame(resultado_vp[["result"]][8],
                           resultado_vp[["result"]][9],
                           resultado_vp[["result"]][10])
resultado_vp = resultado_vp[c(2,4),]
resultado_vp$categoria = c('141               Não','142               Sim')
resultado_vp$x = 17:18
resultado = resultado %>% add_row(resultado_vp)
resultado
remove(resultado_vp)

#Eleito / não eleito

resultado_eleicao = ci.mean.diff(resposta ~ controle, data = BD_reduzido , digits = 3, na.omit = TRUE, group =BD_reduzido$ds_sit_totalizacao)
resultado_eleicao = data.frame(resultado_eleicao[["result"]][8],
                          resultado_eleicao[["result"]][9],
                          resultado_eleicao[["result"]][10])
resultado_eleicao = resultado_eleicao[c(2,4),]
resultado_eleicao$categoria = c('151            Eleito','152        Não Eleito')
resultado_eleicao$x = 19:20
resultado = resultado %>% add_row(resultado_eleicao)
resultado


#comparar vereador com prefeito/viceprefeito

table(BD_reduzido$ds_cargo)

resultado_eleicao = ci.mean.diff(resposta ~ controle, data = BD_reduzido , digits = 3, na.omit = TRUE, group =BD_reduzido$ds_cargo)
resultado_eleicao = data.frame(resultado_eleicao[["result"]][8],
                               resultado_eleicao[["result"]][9],
                               resultado_eleicao[["result"]][10])
resultado_eleicao = resultado_eleicao[c(2,4),]
resultado_eleicao$categoria = c('161          Prefeito','162          Vereador')
resultado_eleicao$x = 21:22
resultado = resultado %>% add_row(resultado_eleicao)
resultado


# FRAUDE
resultado_fraude = ci.mean.diff(resposta ~ controle, data = BD_reduzido , digits = 3, na.omit = TRUE, group =BD_reduzido$fraude)
resultado_fraude = data.frame(resultado_fraude[["result"]][8],
                              resultado_fraude[["result"]][9],
                              resultado_fraude[["result"]][10])
resultado_fraude = resultado_fraude[c(2,4,6,8),]
resultado_fraude$categoria = c('171          Não sei informar','172          Nenhuma fraude', '173          Pouca fraude', '174          Muita fraude')
resultado_fraude$x = 23:26
resultado = resultado %>% add_row(resultado_fraude)
resultado

#writexl::write_xlsx(resultado,path = "resultado_2.xlsx")

library(ggplot2)
ggplot(resultado, aes(x=categoria, y=m.diff, group=categoria,color=categoria,fill=categoria)) +
  geom_pointrange(aes(ymin = low, y=m.diff ,ymax = upp),linewidth = 2,shape=21, size=1.5)+
  #geom_point(shape=21, size=5, fill='royalblue')+
  geom_hline(yintercept = 0,linetype='dashed', col = "#3f3f3f") +
  #scale_colour_manual(values = c("red",'blue','blue','blue','red','red','red','blue','blue','blue','red','red','red', 'blue','blue','blue', 'red','red','blue','blue','red','red','blue','blue','blue','blue')) +
  #scale_fill_manual(values = c('red','blue','blue','blue','red','red','red','blue','blue','blue','red','red','red', 'blue','blue','blue','red','red','blue','blue','red','red','blue','blue','blue','blue')) +
  coord_flip()+
  theme_classic()+
  theme(legend.position="none")+
  labs(y='Diferença entre as médias',x='')

ggsave("C:/Users/08451589707/Documents/GitHub/experimento_lista/abcp/grafico2_politicas.png",
       width = 17,
       height = 17,
       units = "cm",dpi = 300)



# MESMO RESULTADO
# Two-Sided 95% CI for y1, y2, and y3 by group1
# unknown population variances, unequal variance assumption,split analysis by group2
#ci.mean.diff(Q14_3 ~ experimento, data = banco, digits = 3, na.omit = TRUE, split=banco$v2)




table(BD_reduzido$ds_cargo)







prop.table(table(BD$abaixo_ha_uma_lista_de_itens_com_criterios_que_os_eleitores_usam_para_escolher_o_seu_candidato_a_prefeito_a_com_quantos_desses_criterios_o_a_sr_a_concorda_nao_precisamos_saber_quais_estamos_interessados_apenas_na_quantidade_a_a_honestidade_e_o_preparo_para_o_cargo_do_candidato_b_as_propostas_de_governo_do_candidato_c_o_candidato_ser_a_indicacao_de_alguma_pessoa_de_confianca_do_eleitor_d_o_partido_politico_do_candidato_indique_abaixo_o_numero_de_itens_com_os_quais_o_a_sr_a_concorda_0_nenhum_1_2_3_ou_4,BD$controle,useNA = "no"),2)
table(BD$na_sua_opiniao_as_eleicoes_para_prefeito_possuem_muita_fraude_pouca_fraude_ou_nenhuma_fraude)








