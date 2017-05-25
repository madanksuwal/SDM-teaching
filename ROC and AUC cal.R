# Calculate ROC and AUC
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
######