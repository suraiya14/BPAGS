##install.packages("rJava")
#nstall.packages('rJava', type = 'source', INSTALL_opts='--merge-multiarch')
library(stringr)
#setwd("/home/ec2-user/SageMaker/OS_ML_ADT/")
library(rattle)
library(RWeka)
library(rpart)
library(ROSE)
WPM("refresh-cache")
WPM("install-package", "alternatingDecisionTrees")

WPM("load-package", "alternatingDecisionTrees")
cpath <- "weka/classifiers/trees/ADTree"
ADT <- make_Weka_classifier(cpath)

ADT

WOW(ADT)

#install.packages("reshape2")
#library(reshape2)
df_trainX<-read.csv("D:\\Research Work\\Disertation Project 2\\Raw Data\\featureExtraction\\PearsonCorrFeatureSetAddNewSeq.csv", header = TRUE)
#df_trainX<-read.csv("/Applications/kayes/OS_ADT/ALL\ disease/train_data.csv", header = TRUE)
#df_testX<-read.csv("/Applications/kayes/OS_ADT/ALL\ disease/test_data.csv", header = TRUE)

#df_trainX<-read.csv("/Applications/kayes/OS_ADT/AML\ disease/train_data.csv", header = TRUE)
#df_testX<-read.csv("/Applications/kayes/OS_ADT/AML\ disease/test_data.csv", header = TRUE)

#df_trainX<-read.csv("/Applications/kayes/OS_ADT/MDS\ disease/train_data.csv", header = TRUE)
#df_testX<-read.csv("/Applications/kayes/OS_ADT/MDS\ disease/test_data.csv", header = TRUE)
length(df_trainX)
summary(as.factor(df_trainX$Output))
#df_trainX <- ovun.sample(Output ~ ., data = df_trainX, method = "over", N= 3560, seed = 123)$data
#df_trainX <- ovun.sample(Output ~ ., data = df_trainX, method = "under", N= 1230, seed = 123)$data



#audit.adt <- ADT(Output ~ ., data=subtrainX, control = Weka_control(B =40 ))
#evaluation <- evaluate_Weka_classifier(audit.adt)$details
#evaluation["pctCorrect"]

subtrainX<-df_trainX
subtrainX

subtrainX$Output<-as.factor(subtrainX$Output)
subtrainX$Output

#audit.adt <- ADT(Output ~ ., data=subtrainX)
#audit.adt <- ADT(Output ~ ., data=subtrainX)
#audit.adt


#e <- evaluate_Weka_classifier(audit.adt,
#                             numFolds = 10, complexity = TRUE,
#                              seed = 123, class = TRUE)

#e$details

set.seed(123)
#possiblecValue <- round(seq(from = 1, to = 100, length.out = 200),0)
possiblecValue <- round(seq(from = 5, to = 50, length.out = 55),0)
possiblecValue
numModels <- 50
cValue <- sample(possiblecValue, numModels)
cValue
cValue<-round(cValue)
cValue

pctCorrect <- MAE <- Kappa <- rep(0, numModels)
pctCorrect
for(i in 1:numModels){
  print(i)
  audit.adt <- ADT(Output ~ ., data=subtrainX, control = Weka_control(B =cValue[i]))
  e <- evaluate_Weka_classifier(audit.adt,
                                numFolds = 10, complexity = TRUE,
                                seed = 123, class = TRUE)
  evaluation <- e$details
  evaluation
  pctCorrect[i] <- evaluation["pctCorrect"]
  Kappa[i] <- evaluation["kappa"]
  #MAE[i] <- evaluation["meanAbsoluteError"]
  MAE[i] <- evaluation["rootMeanSquaredError"]
}

ind<-which.min(MAE)
ind
min(MAE)


t <- ADT(Output ~ ., data=subtrainX, control = Weka_control(B =cValue[ind]))
t




#feat<-sub(".*)", "", toString(res))


install.packages("BiocManager")
BiocManager::install("Rgraphviz")

#source("http://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")

library("Rgraphviz")
ff <- tempfile()
write_to_dot(t, ff)
write_to_dot(t, "D:\\Research Work\\Disertation Project 2\\Raw Data\\featureExtraction\\ADT/su_cord.dot")
plot(agread(ff))

