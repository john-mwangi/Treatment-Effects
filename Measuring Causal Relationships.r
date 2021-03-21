feeding_prog <- xlsx::read.xlsx(file = 'diff in diff.xlsx',sheetName = 'Sheet1')

feeding_prog

summary(lm(formula = Score ~ D.tr + D.post + D.tr*D.post, data = feeding_prog))

clinics <- xlsx::read.xlsx('clinics.xlsx',sheetName = 'Sheet1')

clinics

# Average mmortality of the treatment group
mean(x = c(clinics$imrate[clinics$T==1]))

# Average mortality of the control group
mean(x = c(clinics$imrate[clinics$T==0]))

clinics2 <- xlsx::read.xlsx('clinics.xlsx', sheetIndex = 'Sheet2')

clinics2

model_clinics <- glm(formula = T ~ povrate + pcdocs, family = 'binomial', data = clinics2)

df_model_clinics <- broom::tidy(model_clinics)

df_model_clinics

# Return the probability of being in the treatment group instead of 0,1
ps1 <- predict(object = model_clinics, newdata = clinics2[,c(3,4)], type = 'response')

ps1
# ps1 is the propensity score

clinics2 <- cbind(clinics2,ps1)

clinics2

clinics2[clinics2$T==1,]

clinics2[clinics2$T==0,]

library(Matching)

ps_att <- Match(Y = clinics2$imrate, Tr = clinics2$T, X = model_clinics$fitted.values, estimand = 'ATT')

summary(ps_att)

ps_ate <- Match(Y = clinics2$imrate, Tr = clinics2$T, X = model_clinics$fitted.values, estimand = 'ATE')

summary(ps_ate)

# These is the treatment group
ps_att$index.treated

# This is how it was matched to the control group
ps_att$index.control

ps_ate$index.treated
ps_ate$index.control

cbind(clinics2[clinics2$T==1,],match1 = ps_att$index.control)

# Manually compute the new treatment effect

mean(x = c(clinics2$imrate[clinics2$T==1])) - (clinics2$imrate[6]+clinics2$imrate[5]*3)/4

MatchBalance(formul = T ~ povrate + pcdocs, data = clinics2, match.out = ps_att, nboots = 0)

# We now want to check the effect on matching of adding villages with more doctors per capital to our sample
qqplot(clinics2$pcdocs[ps_att$index.control], clinics2$pcdocs[ps_att$index.treated])
abline(coef = c(0,1), col='red')

#It is a good match since most of the point lie on the red line


