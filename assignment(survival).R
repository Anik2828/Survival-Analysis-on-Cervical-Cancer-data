a=read.csv("survival data.csv")
summary(a)
# Kaplan-Meier estimate
library(survival)
km=survfit(formula = Surv(Survival,Status)~1,data = a)
summary(km)
library(ggplot2)
library(survminer)
ggsurvplot(km,xlab = "Time",ylab = "Survaival Probability",main="Kaplan-Meier estimate")
#log rank test
log_rank=survfit(Surv(Survival,Status)~Group,data = a)
summary(log_rank)
#plots
ggsurvplot(log_rank,pval=T,pval.method = T,xlab="Time",ylab="Survaival Probability")
# Not significant
# accept the null 
# survival rate of 2 groups are same
# cox proportional hazard model
fit=coxph(formula = Surv(Survival,Status)~Age+Group,data = a)
summary(fit)
#not significant
#age and Group has no impact on Survival rate.
