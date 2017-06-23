# Ex 4: Maxent in R
# required
library(dismo)  
library(rJava)
# paste the Maxent JAVA (.jar) file into dismo package library in R installation folder
# only run if the maxent.jar file is available, in the right folder
# copy maxent.jar file and run following command, only once
# Does not work in Mac
jar <- paste(system.file(package="dismo"), "java/maxent.jar", sep='')
#/Library/Frameworks/R.framework/Versions/3.3/Resources/library/dismo/java
# set RAM allocation for Maxent
options(java.parameters = "-Xmx3g" ) # set memory

#==============

# Import file with predictor extracted data and lat/long
# Presence file
#### Set a working directory, my code will not work 
setwd ("/Users/madanksuwal/Documents/SDM teaching course")
# check the files and load data files
dir()
presence.df<-read.csv("practice files/Rhododendron presence.csv")
pseuabs.df<-read.csv("practice files/Random_pseudoabsence.csv")
lattice.df<-read.csv("practice files/lattice current climate.csv")

# These data set lacks Presence/Absence column, i.e. 1/0 
# Add with 1/0 column
presence.df$PA<-1 ; head(presence.df)
pseuabs.df$PA <-0 ; head(pseuabs.df)
dim(presence.df)

# Extarct presence points by SPECIES
pre.lepi<-subset(presence.df, species=="lepidotum" )
#pre.lown<-subset(presence.df, species=="lowndesii" )
#pre.cown<-subset(presence.df, species=="cowanianum" )


# remove extra columns
names(pre.lepi)
pre.lepi<- pre.lepi[, c(28,4:22)]; names(pre.lepi)
#pre.lown<- pre.lown[, c(28,4:22)]; names(pre.lown)
#pre.cown<- pre.cown[, c(28,4:22)]; names(pre.cown)
pseuabs.df <- pseuabs.df[, c(28,4:22)]; names(pseuabs.df)
latt.df1 <- lattice.df [, c(4:22)] # Lattice with variables
lat.long<- lattice.df [, c(3,2)]  # lattice Lat.Long
head(latt.df1)

# Combine Presence and pseudoabsence
##### Change presence file name here
Pres.df<- pre.lepi
abs.df<- pseuabs.df
PA.df<- rbind(Pres.df, abs.df)
dim(PA.df)
table(PA.df$PA)

P<- PA.df[,2:20]  # Predictors
O<- PA.df[, 1]    # Presence-Absence

==========================
# Model setting
mx.model <- maxent(P, O, args=c("betamultiplier=1",    # Regularizer Multiplier
                                "replicatetype=Subsample", # other options, CV, bootstrapping
                                'randomtestpoints=30', # Test percentage when subsampling is set
                                "replicates=10",        # choose required value
                                "randomseed=true" )    # Must be True when Replicate is set
                   )  
# Store data here
output<- data.frame()
Eval.df<- data.frame()

i=1  # change according to Beta value
output[i, 1]<- i
output[i, 2]<- mx.model@results[, ncol(mx.model@results)][5] # Training AUC
output[i, 3]<- mx.model@results[, ncol(mx.model@results)][8] # Test AUC
output[i, 4]<- mx.model@results[, ncol(mx.model@results)][9] # AUC.SD
output[i, 5]<- mx.model@results[, ncol(mx.model@results)][49] # Entropy
output[i, 6]<- mx.model@results[, ncol(mx.model@results)][69] # Minimum.training.presence.cumulative.threshold 
output[i, 7]<- mx.model@results[, ncol(mx.model@results)][70] # Minimum.training.presence.logistic.threshold
output[i, 8]<- mx.model@results[, ncol(mx.model@results)][76] # Minimum.training.presence.cumulative.threshold

# Rename columns
colnames(output)<- c("Beta.Regularizer",   
                     "Training.AUC", "Test.AUC", "AUC.SD", 
                     "Entroy", "Minimum.training.presence.cumulative.threshold" , 
                     "Minimum.training.presence.logistic.threshold", 
                     "Minimum.training.presence.cumulative.threshold")

# Prediction from each model of K-fold
P.1<-lat.long
for(j in 1:10){  # j = number of replications 
  mx.model@models[[j]]-> m1 # Extract single model from stake of K-fold replications
  P.1[, j+2]<- predict(m1, latt.df1) # Store after lat/long i.e. 3rd column onward
}
# Take Average of each predictions
P.1$B1.avg<-rowSums(P.1[, 3:12])/10 # take average of all predictions, change values
head(P.1)

# Predict in True Presence and pseudoabsence 
TP<-data.frame()
TA<- data.frame()
for (j in 1:10){  # j= number of replications
  TP[1:nrow (Pres.df),j] <- predict(mx.model@models[[j]], Pres.df, args=c("outputformat=raw"))
  TA[1:nrow (abs.df),j]  <- predict(mx.model@models[[j]], abs.df, args=c("outputformat=raw"))
}
# Take average of 10 replications
testP<- rowMeans(TP[, 1:10]) # change value according to j
testA<- rowMeans(TA[, 1:10])

# Evaluate for ROC plot, which will be averag of replications 
Eva.1 <- evaluate(p=testP, a=testA)
i = 1
Eva.1
Eval.df[i, 1]<- Eva.1@auc           # AUC 
Eval.df[i, 2]<- Eva.1@cor           # COR
threshold(Eva.1)
Eval.df[i, 3]<- threshold(Eva.1)[[1]] # Kappa
Eval.df[i, 4]<- threshold(Eva.1)[[5]] # Equal sensitivity specificity
Eval.df[i, 5]<- threshold(Eva.1)[[6]] # Sensitivity

#==================================================================
# ROC plot
#x11()
plot(Eva.1, "ROC")
plot(Eva.1, 'ROC', col=4, pch="", lwd=2, main="")
par(lty=3, lwd=2)


#=================
# Additional arguments are
args=c(
  'maximumbackground=10000',
  'defaultprevalence=1.00',
  'betamultiplier=0.5',
  'pictures=true',
  'randomtestpoints=30',
  'linear=true',
  'quadratic=true',
  'product=true',
  'threshold=true',
  'hinge=true',
  'threads=2',
  'responsecurves=false',
  'jackknife=false',
  'askoverwrite=false'
)
