
#----------------------------------------------------------------------------------
# Two-sample design
#----------------------------------------------------------------------------------
library(misty)

# Two-Sided 95% CI for y1 by group1
# unknown population variances, unequal variance assumption
# print results with 3 digits
resultado1 = ci.mean.diff(Q14_3 ~ experimento, data = banco, digits = 3, na.omit = TRUE)
resultado11 = data.frame(resultado1[["result"]][7],
                         resultado1[["result"]][8],
                         resultado1[["result"]][9])
resultado11 = resultado11[2,]
resultado11$categoria = '1 - global'
resultado11

library(ggplot2)
ggplot(resultado11, aes(x=1, y=m.diff, group=1)) +
  geom_pointrange(aes(ymin = low, ymax = upp), color='#00539C', fill='#00539C',linewidth = 2)+
  geom_point(shape=21, size=5, fill='#00539C')+
  geom_hline(yintercept = 0,linetype='dashed', col = 'red') + 
  coord_flip()+
  theme_classic()


# Two-Sided 95% CI for y1, y2, and y3 by group1
# unknown population variances, unequal variance assumption,
# analysis by group2 separately
haven::print_labels(banco$v2)
resultado_sexo = ci.mean.diff(Q14_3 ~ experimento, data = banco, digits = 3, na.omit = TRUE, group =banco$v2)
resultado_sexo1 = data.frame(resultado_sexo[["result"]][8],
                             resultado_sexo[["result"]][9],
                             resultado_sexo[["result"]][10])
resultado_sexo1 = resultado_sexo1[c(2,4),]
resultado_sexo1$categoria = c('2 - masculino','3 - feminino')


resultado = resultado11 %>% add_row(resultado_sexo1)
resultado$x = 1:3

ggplot(resultado, aes(x=categoria, y=m.diff, group=categoria,color=categoria,fill=categoria)) +
  geom_pointrange(aes(ymin = low, y=m.diff ,ymax = upp),linewidth = 2,shape=21, size=1.5)+
  #geom_point(shape=21, size=5, fill='royalblue')+
  geom_hline(yintercept = 0,linetype='dashed', col = 'red') +
  scale_colour_manual(values = c("blue", "#FDE725FF","red")) +
  scale_fill_manual(values = c("blue", "#FDE725FF","red")) +
  coord_flip()+
  theme_classic()+
  labs(y='diferença entre as médias',x='',
       subtitle = 'Gráfico 1 - Diferença no número médio de itens escolhidos do experimento em lista entre os grupos de tratamento e controle por variáveis sociais e políticas.
Barras Horizontais representam o intervalo com 95% de confiança')

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



