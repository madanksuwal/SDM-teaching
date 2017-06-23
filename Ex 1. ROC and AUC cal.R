# Ex 1. Calculate ROC and AUC
# Required
install.packages(pROC)
library(pROC)
data(aSAH)
aSAH->mydata
head(mydata)
# Fit a GLM
m.fit<- glm(gender~wfns+s100b+ndka , data=mydata, binomial)
mydata$prob <- predict(m.fit,type=c("response"))
gp <- roc(gender ~ prob, data = mydata)
plot(gp) 
auc(gp)

## Kind of Partial plot
library(effects)
g.eff<- allEffects(m.fit) # generate effects of all predictors
plot(g.eff) # plot effects of all variables separately
# plot single desired or interaction 
plot(effect("wfns:ndka", m.fit), rescale.axis=FALSE, ylab="Probability")
