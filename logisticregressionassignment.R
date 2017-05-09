rm(list=ls())
setwd("C:/Users/Sumana/Documents/logistic_regression")
list.files()
NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels
str(NH11$hypev)
levels(NH11$hypev)
NH11$hypev <- factor(NH11$hypev, levels=c("2 No", "1 Yes"))
hyp.out <- glm(hypev~age_p+sex+sleep+bmi,data=NH11, family="binomial")
coef(summary(hyp.out))
hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[, "Estimate"] <- exp(coef(hyp.out))
hyp.out.tab
#creating predictors
predDat <- with(NH11,expand.grid(age_p = c(33,63),sex = "2 Female",bmi = mean(bmi, na.rm = TRUE),sleep = mean(sleep, na.rm = TRUE)))
cbind(predDat,predict(hyp.out, type = "response", se.fit = TRUE,interval = "confidence",newdata = predDat))