
x<-"D:\\Research_Work\\Disertation_Project_2\\Raw Data\\featureExtraction\\reviewer_response\\test_result\\ADT\\selected_training_merged_file.csv"
y<-"D:\\Research_Work\\Disertation_Project_2\\Raw Data\\featureExtraction\\reviewer_response\\test_result\\ADT\\selected_validation_merged_file.csv"
#x<-"D:\\Research_Work\\Disertation_Project_2\\Raw Data\\featureExtraction\\reviewer_response\\ADT\\PSSM\\selected_training_merged_file.csv"
#y<-"D:\\Research_Work\\Disertation_Project_2\\Raw Data\\featureExtraction\\reviewer_response\\ADT\\PSSM\\selected_validation_merged_file.csv"

#x<-"D:\\Research_Work\\Disertation_Project_2\\Raw Data\\featureExtraction\\reviewer_response\\LinearSVC\\PSSM\\selected_training_merged_file.csv"
#y<-"D:\\Research_Work\\Disertation_Project_2\\Raw Data\\featureExtraction\\reviewer_response\\LinearSVC\\PSSM\\selected_validation_merged_file.csv"

#predict_results<- function(x,y) {
#install.packages("cli")
#library(caret)

#R.version.string


#library(ROSE) 
library(e1071)
library(caret)
library(ROCR)
library(mlbench)
#library(pROC)
#library(klaR)
library(mltools)
library(randomForest)
library(pROC)



file_n<-x
#file_n1<-x1
file_other<-y

data1 <- read.csv(file_n, header = TRUE)
data1$Output
#data12 <- read.csv(file_n1, header = TRUE)
#data1<-rbind(data1,data12)
cols<-ncol(data1)

data_backup<-data1

nrows_training<-nrow(data1)

data_other <- read.csv(file_other, header = TRUE)
data_other_backup<-data_other
data_other2<-data_other
cols2<-ncol(data_other)


which(apply(data_other, 2, var) == 0)

#data_other<-data_other[-c(cols2)]
DF2<-data_other
data_other_backup<-data_other
nrows_testing<-nrow(data_other)
nrows_testing

#print(nrows_training)
data1<-data1[-c(cols)]
DF1<-data1
#summary(data)




#if(nrows_testing<100){
#  data_other<-rbind(DF2,DF1)
#}

library(caret)
library(randomForest)


#data_norm<-as.data.frame(scale(data1))
data_norm<-preProcess(data1,method=c("center", "scale"))
#data_norm$mean
data1<-predict(data_norm, data1)

data1["Output"]<-data_backup[,cols]
#data1$Output <- as.factor(data1$Output)
data_other<-predict(data_norm, data_other)



# if(nrows_testing<100){
#   data_other <- data_other[-((nrows_testing+1):nrows_testing_scale), ]
# }
# nrow(data_other)




train<-data1
test<-data_other

suraiya<-123
set.seed(suraiya)
train$Output
#train$Output[train$Output==1]<-"Yes"
train$Output<-as.factor(train$Output)
#tmodel2<-tune(svm, Output~., data = train, ranges = list(epsilon =seq(0,1,0.1), cost=2^(2:7)))
#tmodel2$best.parameters
tmodel2<- tune(randomForest, Output ~ ., data = train, probability = TRUE, ranges= list(ntree = c(500,400), mtry = c(6,5)))
tmodel2$best.parameters
mymodel2<-tmodel2$best.model
mymodel2


results<-predict(mymodel2, test)
results
Prob<-predict(mymodel2, test, type = "prob")
Prob
write.csv(Prob, file = "D:\\Research_Work\\Disertation_Project_2\\Raw Data\\featureExtraction/reviewer_response/test_result\\ADT\\probability_RF.csv", row.names = FALSE)
Prob<-Prob[,"1"]

#AUC calculation
#roc_curve <- roc(test$Output, as.numeric(results))
roc_curve <- roc(test$Output, Prob)
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

test$Output<-as.factor(test$Output)
test$Output

mcc(test$Output, results)
round(mcc(test$Output, results),4)


library(plyr)
data_other_backup$Output<-as.factor(data_other_backup$Output)
results
results<- revalue(results, c("1"="Yes"))
results
results<- revalue(results, c("-1"="No"))
results
sum(results=="Yes")




confusionMatrix(predict(mymodel2,test), test$Output, positive = '1', mode = "everything")

y_pred = predict(mymodel2, newdata = test)

preds <- predict(mymodel2,test)
preds
actuals <- test$Output
actuals

# Calculate MCC
confusion_matrix <- confusionMatrix(predict(mymodel2,test),  test$Output)
confusion_matrix
draw_confusion_matrix <- function(cm) {
  layout(matrix(1))
  par(mar=c(2,2,2,2))
  
  # Create the matrix
  rect(200, 350, 300, 400, col='#AF97D0')
  rect(300, 350, 400, 400, col='#A7AD50')
  text(250, 405, "Reference", cex=1.3, font=2)
  text(175, 375, "Prediction", cex=1.3, srt=90, font=2)
  
  # Add in the cm results
  res <- as.numeric(cm$table)
  text(250, 375, "-1", cex=1.3, font=2, col='black')
  text(250, 350, "1", cex=1.3, font=2, col='black')
  text(300, 400, "-1", cex=1.3, font=2, col='black')
  text(300, 375, res[1], cex=1.6, font=2, col='black')
  text(300, 350, res[3], cex=1.6, font=2, col='black')
  text(400, 400, "1", cex=1.3, font=2, col='black')
  text(400, 375, res[2], cex=1.6, font=2, col='black')
  text(400, 350, res[4], cex=1.6, font=2, col='black')
}


draw_confusion_matrix <- function(cm) {
  
  layout(matrix(1))
  par(mar=c(2,2,2,2))
  #plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  plot(c(123, 345), c(300, 452), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  #title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#AF97D0')
  text(195, 435, -1, cex=1.2)
  rect(250, 430, 340, 370, col='#A7AD50')
  text(295, 435, 1, cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#A7AD50')
  rect(250, 305, 340, 365, col='#AF97D0')
  text(140, 400, -1, cex=1.2, srt=90)
  text(140, 335, 1, cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='black')
  text(195, 335, res[2], cex=1.6, font=2, col='black')
  text(295, 400, res[3], cex=1.6, font=2, col='black')
  text(295, 335, res[4], cex=1.6, font=2, col='black')
  
  
  
}  
draw_confusion_matrix(confusion_matrix)

