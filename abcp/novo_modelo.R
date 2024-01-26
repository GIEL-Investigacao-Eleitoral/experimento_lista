#-----------------------------------------------------
#                    Carregar
#-----------------------------------------------------

library(dplyr)
library(haven)
library(list)
library(readxl)
library(janitor)
BD <- read_excel("C:/Users/08451589707/Desktop/pasta_pessoal/Borba_Vinicius_experimento_lista/BDconcatenado2.xlsx") %>% clean_names()
head(BD)
table(BD$abaixo_ha_uma_lista_de_itens_com_criterios_que_os_eleitores_usam_para_escolher_o_seu_candidato_a_prefeito_a_com_quantos_desses_criterios_o_a_sr_a_concorda_nao_precisamos_saber_quais_estamos_interessados_apenas_na_quantidade_a_a_honestidade_e_o_preparo_para_o_cargo_do_candidato_b_as_propostas_de_governo_do_candidato_c_o_candidato_ser_a_indicacao_de_alguma_pessoa_de_confianca_do_eleitor_d_o_partido_politico_do_candidato_indique_abaixo_o_numero_de_itens_com_os_quais_o_a_sr_a_concorda_0_nenhum_1_2_3_ou_4)

table(BD$controle)

table(BD$abaixo_ha_uma_lista_de_itens_com_criterios_que_os_eleitores_usam_para_escolher_o_seu_candidato_a_prefeito_a_com_quantos_desses_criterios_o_a_sr_a_concorda_nao_precisamos_saber_quais_estamos_interessados_apenas_na_quantidade_a_a_honestidade_e_o_preparo_para_o_cargo_do_candidato_b_as_propostas_de_governo_do_candidato_c_o_candidato_ser_a_indicacao_de_alguma_pessoa_de_confianca_do_eleitor_d_o_partido_politico_do_candidato_indique_abaixo_o_numero_de_itens_com_os_quais_o_a_sr_a_concorda_0_nenhum_1_2_3_ou_4,BD$controle)

BD_reduzido = BD %>% select(abaixo_ha_uma_lista_de_itens_com_criterios_que_os_eleitores_usam_para_escolher_o_seu_candidato_a_prefeito_a_com_quantos_desses_criterios_o_a_sr_a_concorda_nao_precisamos_saber_quais_estamos_interessados_apenas_na_quantidade_a_a_honestidade_e_o_preparo_para_o_cargo_do_candidato_b_as_propostas_de_governo_do_candidato_c_o_candidato_ser_a_indicacao_de_alguma_pessoa_de_confianca_do_eleitor_d_o_partido_politico_do_candidato_indique_abaixo_o_numero_de_itens_com_os_quais_o_a_sr_a_concorda_0_nenhum_1_2_3_ou_4,controle)
BD_reduzido = BD_reduzido %>% rename(resposta = abaixo_ha_uma_lista_de_itens_com_criterios_que_os_eleitores_usam_para_escolher_o_seu_candidato_a_prefeito_a_com_quantos_desses_criterios_o_a_sr_a_concorda_nao_precisamos_saber_quais_estamos_interessados_apenas_na_quantidade_a_a_honestidade_e_o_preparo_para_o_cargo_do_candidato_b_as_propostas_de_governo_do_candidato_c_o_candidato_ser_a_indicacao_de_alguma_pessoa_de_confianca_do_eleitor_d_o_partido_politico_do_candidato_indique_abaixo_o_numero_de_itens_com_os_quais_o_a_sr_a_concorda_0_nenhum_1_2_3_ou_4)
BD_reduzido$resposta=gsub('NA',NA,BD_reduzido$resposta)
BD_reduzido = na.omit(BD_reduzido)
class(BD_reduzido$resposta)

BD_reduzido$resposta = as.numeric(BD_reduzido$resposta)

class(BD_reduzido)
BD_reduzido = data.frame(BD_reduzido)
#----------------------------------------------------------------------------------------
#Calculate list experiment difference in means
diff_medias <- ictreg(resposta ~ 1, data = BD_reduzido, 
                      treat = "controle", J=4, method = "lm")
summary(diff_medias)

aaa <- predict(diff_medias, newdata = as.data.frame(matrix(1, 1, 1)), se.fit = TRUE, 
               avg = TRUE)
aaa
remove(aaa)

list::ict.test()
list::ict.test(BD_reduzido$resposta,BD_reduzido$controle,J=4)
