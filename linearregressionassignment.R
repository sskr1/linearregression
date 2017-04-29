rm(list=ls())
states.data <- readRDS("states.rds")
View(states.data)
#get labels -- why do we use attributes?
states.info <- data.frame(attributes(states.data)[c("names","var.labels")])
tail(states.info,8)
#summary of expense and csat columns, all rows
 sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary (sts.ex.sat)
#correlaton between expense and csat
cor(sts.ex.sat)
# scatter plot of expense vs csat
 plot(sts.ex.sat)
 sat.mod <- lm(csat~expense, data = states.data)
 summary(sat.mod)
 summary(lm(csat ~ expense + percent, data = states.data))
 plot(states.data$percent)
plot(states.data$pop,states.data$percent)
class(sat.mod)
 names(sat.mod)
methods(class = class(sat.mod))[1:9]
#what is the purpose of confint in this instance? 
confint(sat.mod)
#2.5%        97.5 %
# iercept) 995.01753164 1126.44735626
# expense      -0.03440768   -0.01014361
 hist(residuals(sat.mod))
 ?confint
 par(mar = c(4, 4, 2, 2), mfrow = c(1, 2))
 plot(sat.mod, which = c(1, 2))
 summary(lm(csat ~ expense + house + senate, data = states.data))
 summary(lm(csat ~ expense + house, data = states.data))
 sat.voting.mod <-  lm(csat ~ expense + house + senate,data = na.omit(states.data))
 sat.mod <- update(sat.mod, data = na.omit(states.data))
#Why ANOVA?
anova(sat.mod,sat.voting.mod)
#Analysis of Variance Table
 summary(lm(energy~metro, data = na.omit(states.data)))
 plot(states.data$energy, states.data$metro)
 energymodel = lm(energy~metro, data = na.omit(states.data))
 plot(energymodel$residuals)
 hist(energymodel$residuals)
plot(energymodel$residuals)
 plot(energymodel)
 energymodelnew2 = lm(energy~pop+green, data = na.omit(states.data))
 summary(energymodelnew2)
 energymodelnew3 = lm(energy~pop+green+miles, data = na.omit(states.data))
 summary(energymodelnew3)
 energymodelnew4 = lm(energy~pop+green+miles+metro, data = na.omit(states.data))
 summary(energymodelnew4)
 #interactions
 sat.expense.by.percent <- lm(csat ~ expense*income,data=states.data) 
 coef(summary(sat.expense.by.percent))
 str(states.data$region)
 states.data$region <- factor(states.data$region)
 sat.region <- lm(csat~region,data = states.data)
 coef(summary(sat.region))
 anova(sat.region)
 contrasts(states.data$region)
 coef(summary(lm(csat ~ C(region, base=4),data=states.data)))
 coef(summary(lm(csat ~ C(region, contr.helmert),data=states.data)))
 # Add on to the regression equation that you created in exercise 1 by generating an interaction term and testing the interaction.
 # Add region to the model. Are there significant differences across the four regions.
 sat.model.new = lm(csat ~ expense + income + expense*income +region, data = na.omit(states.data))
 summary(sat.model.new)
 
 
                              
                              