load("C:/Users/08451589707/Documents/GitHub/experimento_lista/anpocs/BD_Covid_Faperj_Anpocs.RData")
remove(dicionario)
#----------------------------------------------------------------------------------
# Two-sample design
#----------------------------------------------------------------------------------
library(misty)
library(dplyr)

# Two-Sided 95% CI for y1 by group1
# unknown population variances, unequal variance assumption
# print results with 3 digits
resultado1 = ci.mean.diff(Q14_3 ~ experimento, data = banco, digits = 3, na.omit = TRUE)
resultado11 = data.frame(resultado1[["result"]][7],
                         resultado1[["result"]][8],
                         resultado1[["result"]][9])
resultado11 = resultado11[2,]
resultado11$categoria = '100 global'
resultado11

#library(ggplot2)
#ggplot(resultado11, aes(x=1, y=m.diff, group=1)) +
#  geom_pointrange(aes(ymin = low, ymax = upp), color='#00539C', fill='#00539C',linewidth = 2)+
#  geom_point(shape=21, size=5, fill='#00539C')+
#  geom_hline(yintercept = 0,linetype='dashed', col = 'red') + 
#  coord_flip()+
#  theme_classic()


# Two-Sided 95% CI for y1, y2, and y3 by group1
# unknown population variances, unequal variance assumption,
# analysis by group2 separately
haven::print_labels(banco$v2)
resultado_sexo = ci.mean.diff(Q14_3 ~ experimento, data = banco, digits = 3, na.omit = TRUE, group =banco$v2)
resultado_sexo1 = data.frame(resultado_sexo[["result"]][8],
                             resultado_sexo[["result"]][9],
                             resultado_sexo[["result"]][10])
resultado_sexo1 = resultado_sexo1[c(2,4),]
resultado_sexo1$categoria = c('101 masculino','102 feminino')
resultado = resultado11 %>% add_row(resultado_sexo1)
resultado$x = 1:3


# idade2
haven::print_labels(banco$faixaidade2)
resultado_idade = ci.mean.diff(Q14_3 ~ experimento, data = banco, digits = 3, na.omit = TRUE, group =banco$faixaidade2)
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

# Raça2
haven::print_labels(banco$Raça2)
resultado_raca = ci.mean.diff(Q14_3 ~ experimento, data = banco, digits = 3, na.omit = TRUE, group =banco$Raça2)
resultado_raca[["result"]]
resultado_raca = data.frame(resultado_raca[["result"]][8],
                            resultado_raca[["result"]][9],
                            resultado_raca[["result"]][10])
resultado_raca = resultado_raca[c(2,4,6),]
resultado_raca$categoria = c('109 - Preta/Parda','110 - Branca',
                              '111 - Outras')
resultado_raca
resultado_raca$x = 9:11
resultado = resultado %>% add_row(resultado_raca)
remove(resultado_idade,resultado_raca,resultado_sexo1,resultado11,resultado1,resultado_sexo)
# Educa
haven::print_labels(banco$escolaridade2)
table(banco$escolaridade2)

resultado_educa = ci.mean.diff(Q14_3 ~ experimento, data = banco, digits = 3, na.omit = TRUE, group =banco$escolaridade2)
resultado_educa[["result"]]
resultado_educa = data.frame(resultado_educa[["result"]][8],
                            resultado_educa[["result"]][9],
                            resultado_educa[["result"]][10])
resultado_educa = resultado_educa[c(2,4,6),]
resultado_educa$categoria = c('112 Até Ensino Fundamental','113 Até Ensino Médio',
                             '114 Ensino Superior')
resultado_educa
resultado_educa$x = 12:14
resultado = resultado %>% add_row(resultado_educa)
remove(resultado_educa)

# Renda2
haven::print_labels(banco$renda2)
table(banco$renda2)

resultado_renda2 = ci.mean.diff(Q14_3 ~ experimento, data = banco, digits = 3, na.omit = TRUE, group =banco$renda2)
resultado_renda2[["result"]]
resultado_renda2 = data.frame(resultado_renda2[["result"]][8],
                             resultado_renda2[["result"]][9],
                             resultado_renda2[["result"]][10])
resultado_renda2 = resultado_renda2[c(2,4,6,8),]
resultado_renda2$categoria = c('115 Até 02','116 02 a 05',
                              '117 05 a 10','118 Mais de 10')
resultado_renda2
resultado_renda2$x = 15:18
resultado = resultado %>% add_row(resultado_renda2)
remove(resultado_renda2)

# Religião
haven::print_labels(banco$religião2)
table(banco$religião2)

resultado_religião2 = ci.mean.diff(Q14_3 ~ experimento, data = banco, digits = 3, na.omit = TRUE, group =banco$religião2)
resultado_religião2[["result"]]
resultado_religião2 = data.frame(resultado_religião2[["result"]][8],
                              resultado_religião2[["result"]][9],
                              resultado_religião2[["result"]][10])
resultado_religião2 = resultado_religião2[c(2,4,6,8),]
resultado_religião2$categoria = c('119 Católica','120 Evangélica',
                               '121 Outras religiões','122 Sem religião')
resultado_religião2
resultado_religião2$x = 19:22
resultado = resultado %>% add_row(resultado_religião2)





# região3
haven::print_labels(banco$região3)
table(banco$região3)

resultado_região3 = ci.mean.diff(Q14_3 ~ experimento, data = banco, digits = 3, na.omit = TRUE, group =banco$região3)
resultado_região3[["result"]]
resultado_região3 = data.frame(resultado_região3[["result"]][8],
                                 resultado_região3[["result"]][9],
                                 resultado_região3[["result"]][10])
resultado_região3 = resultado_região3[c(2,4,6),]
resultado_região3$categoria = c('123 Capital','124 Metropolitana',
                                  '125 Interior')
resultado_região3
resultado_região3$x = 23:25
resultado = resultado %>% add_row(resultado_região3)

remove(resultado_região3,resultado_religião2)



ggplot(resultado, aes(x=categoria, y=m.diff, group=categoria,color=categoria,fill=categoria)) +
  geom_pointrange(aes(ymin = low, y=m.diff ,ymax = upp),linewidth = 2,shape=21, size=1.5)+
  #geom_point(shape=21, size=5, fill='royalblue')+
  geom_hline(yintercept = 0,linetype='dashed', col = 'red') +
  scale_colour_manual(values = c("#00539C", "#B22222","#B22222","#00539C", "#00539C","#00539C","#00539C","#00539C", "#B22222","#B22222","#B22222","#00539C", "#00539C","#00539C", "#B22222", "#B22222", "#B22222", "#B22222", "#00539C", "#00539C","#00539C","#00539C","#B22222","#B22222","#B22222")) +
  scale_fill_manual(values = c("#00539C", "#B22222","#B22222","#00539C", "#00539C","#00539C","#00539C","#00539C", "#B22222","#B22222","#B22222","#00539C", "#00539C","#00539C", "#B22222", "#B22222", "#B22222", "#B22222","#00539C", "#00539C","#00539C","#00539C","#B22222","#B22222","#B22222")) +
  coord_flip()+
  theme_classic()+
  theme(legend.position="none")+
  labs(y='diferença entre as médias',x='',
       subtitle = 'Gráfico 1 - Diferença no número médio de itens escolhidos do experimento em lista entre os 
       grupos de tratamento e controle por variáveis sociais.
       Barras Horizontais representam o intervalo com 95% de confiança')

ggsave("C:/Users/08451589707/Documents/GitHub/experimento_lista/anpocs/grafico1.png")

#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------------

resultado_geral = ci.mean.diff(Q14_3 ~ experimento, data = banco, digits = 3, na.omit = TRUE)
resultado_geral = data.frame(resultado_geral[["result"]][7],
                             resultado_geral[["result"]][8],
                             resultado_geral[["result"]][9])
resultado_geral = resultado_geral[2,]
resultado_geral$categoria = '100 global'
resultado_geral$x = 0
resultado_geral

# Importância do voto (Q5)
haven::print_labels(banco$Q5)
attributes(banco$Q5)$label
banco_q5 = banco %>% filter(Q5!=99)
resultado_Q5 = ci.mean.diff(Q14_3 ~ experimento, data = banco_q5, digits = 3, na.omit = TRUE, group =banco_q5$Q5)
resultado_Q5 = data.frame(resultado_Q5[["result"]][8],
                             resultado_Q5[["result"]][9],
                             resultado_Q5[["result"]][10])
resultado_Q5 = resultado_Q5[c(2,4,6),]
resultado_Q5$categoria = c('101 Muito importante','102 Pouco importante','103 Nada importante')
resultado_Q5$x = 1:3
resultado = resultado_geral %>% add_row(resultado_Q5)

remove(banco_q5,resultado_geral,resultado_Q5)

# Voto Obrigatório (Q7) 
haven::print_labels(banco$Q7)
attributes(banco$Q7)
table(banco$Q7)
banco_q7 = banco %>% filter(Q7!=99)
resultado_Q7 = ci.mean.diff(Q14_3 ~ experimento, data = banco_q7, digits = 3, na.omit = TRUE, group =banco_q7$Q7)
resultado_Q7 = data.frame(resultado_Q7[["result"]][8],
                          resultado_Q7[["result"]][9],
                          resultado_Q7[["result"]][10])
resultado_Q7 = resultado_Q7[c(2,4),]
resultado_Q7$categoria = c('104 Votaria','105 Não votaria')
resultado_Q7$x = 4:5
resultado = resultado %>% add_row(resultado_Q7)
remove(banco_q5,resultado_geral,resultado_Q5)
remove(banco_q7,resultado_Q7)

# confiança na urna Q8
haven::print_labels(banco$Q8)
attributes(banco$Q8)

table(banco$Q8)
banco_q8 = banco %>% filter(Q8!=99)
resultado_Q8 = ci.mean.diff(Q14_3 ~ experimento, data = banco_q8, digits = 3, na.omit = TRUE, group =banco_q8$Q8)
resultado_Q8 = data.frame(resultado_Q8[["result"]][8],
                          resultado_Q8[["result"]][9],
                          resultado_Q8[["result"]][10])
resultado_Q8 = resultado_Q8[c(2,4,6),]
resultado_Q8$categoria = c('106 Muita confiança','107 Pouca confiança','108 Nenhuma confiança')
resultado_Q8$x = 6:8
resultado = resultado %>% add_row(resultado_Q8)

remove(banco_q8,resultado_Q8)

# ideologia2
haven::print_labels(banco$ideologia2)
table(banco$ideologia2)
banco_ideologia2 = banco %>% filter(ideologia2!=99)
resultado_ideologia2 = ci.mean.diff(Q14_3 ~ experimento, data = banco_ideologia2, digits = 3, na.omit = TRUE, group =banco_ideologia2$ideologia2)
resultado_ideologia2 = data.frame(resultado_ideologia2[["result"]][8],
                          resultado_ideologia2[["result"]][9],
                          resultado_ideologia2[["result"]][10])
resultado_ideologia2 = resultado_ideologia2[c(2,4,6,8),]
resultado_ideologia2$categoria = c('109 Esquerda','110 Centro','111 Direita','112 Nenhuma')
resultado_ideologia2$x = 9:12
resultado = resultado %>% add_row(resultado_ideologia2)

remove(banco_ideologia2,resultado_ideologia2)


ggplot(resultado, aes(x=categoria, y=m.diff, group=categoria,color=categoria,fill=categoria)) +
  geom_pointrange(aes(ymin = low, y=m.diff ,ymax = upp),linewidth = 2,shape=21, size=1.5)+
  #geom_point(shape=21, size=5, fill='royalblue')+
  geom_hline(yintercept = 0,linetype='dashed', col = 'red') +
  scale_colour_manual(values = c("#00539C","#B22222", "#B22222","#B22222","#00539C", "#00539C", "#B22222","#B22222","#B22222","#00539C", "#00539C","#00539C", "#00539C")) +
  scale_fill_manual(values = c("#00539C","#B22222", "#B22222","#B22222","#00539C", "#00539C", "#B22222","#B22222","#B22222","#00539C", "#00539C","#00539C","#00539C")) +
  coord_flip()+
  theme_classic()+
  theme(legend.position="none")+
  labs(y='diferença entre as médias',x='',
       subtitle = 
       'Gráfico 2 - Diferença no número médio de itens escolhidos do experimento em lista entre 
       os grupos de tratamento e controle por variáveis políticas.
       Barras Horizontais representam o intervalo com 95% de confiança')

ggsave("C:/Users/08451589707/Documents/GitHub/experimento_lista/anpocs/grafico2.png")

# MESMO RESULTADO
# Two-Sided 95% CI for y1, y2, and y3 by group1
# unknown population variances, unequal variance assumption,split analysis by group2
#ci.mean.diff(Q14_3 ~ experimento, data = banco, digits = 3, na.omit = TRUE, split=banco$v2)
























#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------
#----------------------------------------------------------------------------------


library(misty)


dat1 <- data.frame(group1 = c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2,
                              1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2),
                   group2 = c(1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2,
                              1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2),
                   group3 = c(1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2,
                              1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2),
                   x1 = c(3, 1, 4, 2, 5, 3, 2, 3, 6, 4, 3, NA, 5, 3,
                          3, 2, 6, 3, 1, 4, 3, 5, 6, 7, 4, 3, 6, 4),
                   x2 = c(4, NA, 3, 6, 3, 7, 2, 7, 3, 3, 3, 1, 3, 6,
                          3, 5, 2, 6, 8, 3, 4, 5, 2, 1, 3, 1, 2, NA),
                   x3 = c(7, 8, 5, 6, 4, 2, 8, 3, 6, 1, 2, 5, 8, 6,
                          2, 5, 3, 1, 6, 4, 5, 5, 3, 6, 3, 2, 2, 4))


#--------------------------------------
# Two-sample design

# Two-Sided 95% CI for y1 by group1
# unknown population variances, unequal variance assumption
# print results with 3 digits
ci.mean.diff(x1 ~ group1, data = dat1, digits = 3)

# Two-Sided 95% CI for y1, y2, and y3 by group1
# unknown population variances, unequal variance assumption,
# listwise deletion for missing data
ci.mean.diff(cbind(x1, x2, x3) ~ group1, data = dat1, na.omit = TRUE)

# Two-Sided 95% CI for y1, y2, and y3 by group1
# unknown population variances, unequal variance assumption,
# analysis by group2 separately
ci.mean.diff(cbind(x1, x2, x3) ~ group1, data = dat1, group = dat1$group2)

# Two-Sided 95% CI for y1, y2, and y3 by group1
# unknown population variances, unequal variance assumption,
# split analysis by group2
ci.mean.diff(cbind(x1, x2, x3) ~ group1, data = dat1, split = dat1$group2)

# Two-Sided 95% CI for y1, y2, and y3 by group1
# unknown population variances, unequal variance assumption,
# analysis by group2 separately, split analysis by group3
ci.mean.diff(cbind(x1, x2, x3) ~ group1, data = dat1,
             group = dat1$group2, split = dat1$group3)



