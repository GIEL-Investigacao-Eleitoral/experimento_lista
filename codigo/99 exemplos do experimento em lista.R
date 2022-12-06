# https://imai.fas.harvard.edu/projects/sensitive.html
# https://imai.fas.harvard.edu/research/list.html
# https://imai.fas.harvard.edu/research/listP.html

# https://imai.fas.harvard.edu/software/list.html
# https://imai.fas.harvard.edu/software/rr.html
# https://imai.fas.harvard.edu/software/endorse.html

library(list)
list::ict.test()
list::ict.hausman.test()
list::ictreg()
list::ictreg.joint()
list::ictregBayes()
list::mexico
list::predict.ictreg()

?list::combinedListDirect()
  
  
data(affirm)
data(race)
summary(affirm)
summary(race)

# Conduct test with null hypothesis that there is no design effect
# Replicates results on Blair and Imai (2012) pg. 69

test.value.affirm <- ict.test(affirm$y, affirm$treat, J = 3, gms = TRUE)
print(test.value.affirm)

test.value.race <- ict.test(race$y, race$treat, J = 3, gms = TRUE)
print(test.value.race)
#------------------------------------------------------------------------------
# complete case analysis
combinedListExps <- na.omit(combinedListExps)
# Conduct estimation without covariate adjustment
out.1 <- combinedListDirect(list1N ~ list1treat, 
                            data = subset(combinedListExps, directsfirst==1), 
                            treat = "list1treat", direct = "direct1")
summary(out.1)
# Conduct estimation with covariate adjustment
out.2 <- combinedListDirect(list1N ~ list1treat + gender + 
                              ideology + education + race, 
                            data = subset(combinedListExps, directsfirst==1), 
                            treat = "list1treat", direct = "direct1")
summary(out.2)
#------------------------------------------------------------------------------
data(race)
set.seed(1)
# Calculate list experiment difference in means
diff.in.means.results <- ictreg(y ~ 1, data = race, 
                                treat = "treat", J=3, method = "lm")

summary(diff.in.means.results)
# Fit linear regression
# Replicates Table 1 Columns 1-2 Imai (2011); note that age is divided by 10
lm.results <- ictreg(y ~ south + age + male + college, data = race, 
                     treat = "treat", J=3, method = "lm")
summary(lm.results)
# Fit two-step non-linear least squares regression
# Replicates Table 1 Columns 3-4 Imai (2011); note that age is divided by 10
nls.results <- ictreg(y ~ south + age + male + college, data = race, 
                      treat = "treat", J=3, method = "nls")
summary(nls.results)
# Fit EM algorithm ML model with constraint
# Replicates Table 1 Columns 5-6, Imai (2011); note that age is divided by 10
ml.constrained.results <- ictreg(y ~ south + age + male + college, data = race, 
                                 treat = "treat", J=3, method = "ml", 
                                 overdispersed = FALSE, constrained = TRUE)
summary(ml.constrained.results)
# Fit EM algorithm ML model with no constraint
# Replicates Table 1 Columns 7-10, Imai (2011); note that age is divided by 10
ml.unconstrained.results <- ictreg(y ~ south + age + male + college, data = race, 
                                   treat = "treat", J=3, method = "ml", 
                                   overdispersed = FALSE, constrained = FALSE)
summary(ml.unconstrained.results)
# Fit EM algorithm ML model for multiple sensitive items
# Replicates Table 3 in Blair and Imai (2010)
multi.results <- ictreg(y ~ male + college + age + south + south:age, treat = "treat", 
                        J = 3, data = multi, method = "ml", 
                        multi.condition = "level")
summary(multi.results)
# Fit standard design ML model
# Replicates Table 7 Columns 1-2 in Blair and Imai (2010)
noboundary.results <- ictreg(y ~ age + college + male + south, treat = "treat",
                             J = 3, data = affirm, method = "ml", 
                             overdispersed = FALSE)

summary(noboundary.results)

# Fit standard design ML model with ceiling effects alone
# Replicates Table 7 Columns 3-4 in Blair and Imai (2010)

ceiling.results <- ictreg(y ~ age + college + male + south, treat = "treat", 
                          J = 3, data = affirm, method = "ml", fit.start = "nls",
                          ceiling = TRUE, ceiling.fit = "bayesglm",
                          ceiling.formula = ~ age + college + male + south)
summary(ceiling.results)

# Fit standard design ML model with floor effects alone
# Replicates Table 7 Columns 5-6 in Blair and Imai (2010)

floor.results <- ictreg(y ~ age + college + male + south, treat = "treat", 
                        J = 3, data = affirm, method = "ml", fit.start = "glm", 
                        floor = TRUE, floor.fit = "bayesglm",
                        floor.formula = ~ age + college + male + south)

summary(floor.results)

# Fit standard design ML model with floor and ceiling effects
# Replicates Table 7 Columns 7-8 in Blair and Imai (2010)
both.results <- ictreg(y ~ age + college + male + south, treat = "treat", 
                       J = 3, data = affirm, method = "ml", 
                       floor = TRUE, ceiling = TRUE, 
                       floor.fit = "bayesglm", ceiling.fit = "bayesglm",
                       floor.formula = ~ age + college + male + south,
                       ceiling.formula = ~ age + college + male + south)
summary(both.results)

# Response error models (Blair, Imai, and Chou 2018)
top.coded.error <- ictreg(
  y ~ age + college + male + south, treat = "treat",
  J = 3, data = race, method = "ml", error = "topcoded")

uniform.error <- ictreg(
  y ~ age + college + male + south, treat = "treat",
  J = 3, data = race, method = "ml", error = "topcoded")

# Robust models, which constrain sensitive item proportion
#   to difference-in-means estimate
robust.ml <- ictreg(
  y ~ age + college + male + south, treat = "treat",
  J = 3, data = affirm, method = "ml", robust = TRUE)

robust.nls <- ictreg(
  y ~ age + college + male + south, treat = "treat",
  J = 3, data = affirm, method = "nls", robust = TRUE)
## End(Not run)

