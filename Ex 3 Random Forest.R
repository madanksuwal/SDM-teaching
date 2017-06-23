# Ex 3. RandomForest
#### Set a working directory, my code will not work 
setwd ("/Users/madanksuwal/Documents/SDM teaching course")
# check the files and load data files
dir()
pre.df<-read.csv("practice files/Rhododendron presence.csv")
pseu.df<-read.csv("practice files/Random_pseudoabsence.csv")
latt.df<-read.csv("practice files/lattice current climate.csv")
latt70s.df<-read.csv("practice files/Lattice 2070s climate.csv")

dim(pre.df); dim(pseu.df); dim(latt.df); dim(latt70s.df)

# These data set lacks Presence/Absence column, i.e. 1/0 
# Add with 1/0 column
pre.df$PA<-1 ; head(pre.df)
pseu.df$PA<-0 ; head(pseu.df)

# Combind Presence and Psuedo-absence (only Predictory and response columns)
PA.df<- rbind(pre.df[,4:28], pseu.df[,4:28])
dim(PA.df)
PA.df<- PA.df[complete.cases(PA.df),]
dim(PA.df)
names(PA.df)
# data Preparation and Exploration Over by here
#====================================

# Prepare for Model 
library(randomForest)
names(PA.df)
# fit light model with a few variables
m.rf<- randomForest(as.factor(PA)~ bio01+bio02, data=PA.df)
print(m.rf)
plot(m.rf)
legend("topright", col=c("green", "black", "red"), lty=c(3,1,2), bty="n",
       legend=c("Presence", "OOB", "Absence"), title="Error for:")
# Error is MSE (Mean Square Error)
# the above plot is from, Out-of-bag, Prediction error for Pseudo-Absence, and Presence
# at every tree, 1/3 is left out-of-bag, with replacement
head(m.rf$err.rate)

#========================
# Fit full model, with all variables 
# Extract Predictor variable names only
varNames<- colnames(PA.df[,-c(25)]) 
print(varNames)

# add "+" sign between predictor variables 
varNames1 <- paste(varNames, collapse = "+")
print (varNames1)

# Add response variables and convert to a formula object
rf.formula<- as.formula(paste (as.factor("PA"), varNames1, sep= "~"))
print (rf.formula)

# Run RandomForest with above formula
rh.rf<- randomForest(as.factor(PA) ~ bio01, data=PA.df, ntree=1000, importance= TRUE)
names(PA.df)
rh.rf<- randomForest (y= as.factor (PA.df[,25]), x= PA.df[,1:24], data = PA.df, ntree = 1000, importance= TRUE)
# See model output, confusion matrix and class.error
print(rh.rf)
plot(rh.rf)
legend("topright", col=c("green", "black", "red"), lty=c(3,1,2), bty="n",
       legend=c("Presence", "OOB", "Absence"), title="Error for:")
varImpPlot(rh.rf, sort = TRUE, main = "Variable Importance", n.var=10)

# Variable Importance Table
var.imp<- data.frame (importance(rh.rf, type= 2))
var.imp$Variables <- row.names(var.imp)
var.imp[order (var.imp$MeanDecreaseGini, decreasing = TRUE), ]

# Creating performance object
library(ROCR)
a.v<-as.vector(rh.rf$votes[,2]) # extract predicted '1'
perf.obj<- prediction( predictions = a.v, labels = PA.df$PA  )

# Calculate AUC
rh.AUC <- performance(perf.obj, "auc")
AUC=rh.AUC@y.values[[1]]
AUC

# Plot ROC 
rh.ROC <- performance(perf.obj, 'tpr', 'fpr')

plot(rh.ROC, main="ROC Plot", xlab=" 1 - Specificity: False Positive Rate", 
     ylab="Sensitivity: True Positive Rate", lwd=2)
abline(a=0,b=1, lty=3)  # diagonal line
#text(0.5,0.5,  labels=AUC)
legend("bottomright", legend = c("AUC: ", round(AUC,3)), bty="n", ncol=2)

# Response curve or partial plots
partialPlot(rh.rf, PA.df[,1:24], x.var=bio01, plot = T, n.pt = 30, xlim=c(0,300), ylim=c(0,2)) # change n.pt for smooth plot
partialPlot(rh.rf, PA.df[,1:24], x.var=bio03, plot = T, n.pt = 30, add=T, xlim=c(0,300), ylim=c(0,2)) # change n.pt for smooth plot
partialPlot(rh.rf, PA.df[,1:24], x.var=bio04, plot = T, n.pt = 30, add=T, xlim=c(0,300), ylim=c(0,2)) # change n.pt for smooth plot
partialPlot(rh.rf, PA.df[,1:24], x.var=bio12, plot = T, n.pt = 30, xlim=c(0,2000), ylim=c(0,2)) # change n.pt for smooth plot
partialPlot(rh.rf, PA.df[,1:24], x.var=bio19, plot = T, n.pt = 30, add=T, xlim=c(0,2000), ylim=c(0,2)) # change n.pt for smooth plot
partialPlot(rh.rf, PA.df[,1:24], x.var=abt, plot = T, n.pt = 30) # change n.pt for smooth plot


#




