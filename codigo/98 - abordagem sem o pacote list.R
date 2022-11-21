

banco$experimento = ifelse(is.na(banco$Q14_2),"lista pequena","lista grande")
table(banco$experimento)

banco %>% group_by(experimento) %>% summarise(media=mean(Q14_3))

shapiro.test(banco$Q14_3)
wilcox.test(Q14_3 ~ experimento, data = banco)

#The scale parameters are assumed to be unknown 
#and not necessarily equal, and the problem is 
#to assess whether the location parameters can 
#reasonably be treated as equal. Lehmann[1] 
#states that "the Behrens–Fisher problem" is 
#used both for this general form of model when 
#the family of distributions is arbitrary and 
#for when the restriction to a normal distribution 
#is made. While Lehmann discusses a number of 
#approaches to the more general problem, mainly 
#based on nonparametrics,[2] most other sources 
#appear to use "the Behrens–Fisher problem" to 
#refer only to the case where the distribution 
#is assumed to be normal: most of this article 
#makes this assumption. 

library(pairwiseCI)
banco$Q14_1
banco$Q14_2
lista_grande  = subset(banco, is.na(Q14_1))$Q14_2
lista_pequena = subset(banco, is.na(Q14_2))$Q14_1

np.re(x=lista_grande, y=lista_pequena, conf.level = 0.95)

# nonparametric bootstrap
library(nptest)
lista_grande_mediana   <- np.boot(x = lista_grande , statistic = mean)
lista_pequena_mediana  <- np.boot(x = lista_pequena , statistic = mean)

#BCa Confidence Intervals:
#    lower  upper
#90% 2.0066 2.1455
#95% 1.9907 2.1587
#99% 1.9648 2.1842

#BCa Confidence Intervals:
#     lower  upper
#90% 1.8836 2.0078
#95% 1.8719 2.0207
#99% 1.8461 2.0427






