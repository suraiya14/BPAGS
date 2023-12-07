#install.packages("klaR")



library(e1071)
library(caret)
library(mltools)
#x<-"D:\\Research_Work\\Disertation_Project_2\\Raw Data\\featureExtraction\\reviewer_response\\LinearSVC\\BIN\\selected_training_merged_file.csv"
#y<-"D:\\Research_Work\\Disertation_Project_2\\Raw Data\\featureExtraction\\reviewer_response\\LinearSVC\\BIN\\selected_validation_merged_file.csv"
#x<-"D:\\Research_Work\\Disertation_Project_2\\Raw Data\\featureExtraction\\reviewer_response\\GA\\PSSM\\selected_training_merged_file.csv"
#y<-"D:\\Research_Work\\Disertation_Project_2\\Raw Data\\featureExtraction\\reviewer_response\\GA\\PSSM\\selected_validation_merged_file.csv"
x<-"D:\\Research_Work\\Disertation_Project_2\\Raw Data\\featureExtraction\\reviewer_response\\test_result\\ADT\\selected_training_merged_file.csv"
y<-"D:\\Research_Work\\Disertation_Project_2\\Raw Data\\featureExtraction\\reviewer_response\\test_result\\ADT\\selected_validation_merged_file.csv"



file_n<-x
#file_n1<-x1
file_other<-y
file_other

data1 <- read.csv(file_n, header = TRUE)
data1$Output
#data12 <- read.csv(file_n1, header = TRUE)
#data1<-rbind(data1,data12)
cols<-ncol(data1)

data_backup<-data1

nrows_training<-nrow(data1)

data_other <- read.csv(file_other, header = TRUE)
data_other$Output
data_other_backup<-data_other
data_other2<-data_other
cols2<-ncol(data_other)
cols2


which(apply(data_other, 2, var) == 0)

#data_other<-data_other[-c(cols2)]
DF2<-data_other
data_other_backup<-data_other
nrows_testing<-nrow(data_other)
nrows_testing

#print(nrows_training)
data1<-data1[-c(cols)]
DF1<-data1
DF1
#summary(data)




#if(nrows_testing<100){
#  data_other<-rbind(DF2,DF1)
#}

library(caret)
data1$dist_100

#data_norm<-as.data.frame(scale(data1))
data_norm<-preProcess(data1,method=c("center", "scale"))
#data_norm$mean
data1<-predict(data_norm, data1)
data1
data1["Output"]<-data_backup[,cols]
#data1$Output <- as.factor(data1$Output)
data_other<-predict(data_norm, data_other)
data_other$dist_100


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

# Create a range of k values to test
k_values <- c(3, 5, 7, 9, 11)

# Define a parameter grid
param_grid <- expand.grid(k = k_values)

# Specify the train control with cross-validation
ctrl <- trainControl(method = "cv", number = 5)

# Perform hyperparameter tuning
model <- train(Output ~ ., data = train, method = "knn", trControl = ctrl, tuneGrid = param_grid, metric = "Accuracy")

# Print the best k value
print(model$bestTune)

# Retrain the model with the best k value
best_k <- model$bestTune$k
model <- train(Output ~ ., data = train, method = "knn", trControl = ctrl ,tuneGrid = data.frame(k = best_k))

model





# Make predictions on the test data
predictions <- predict(model, newdata = test)
predictions
predictionsP <- predict(model, newdata = test, type="prob")
predictionsP<-predictionsP[,'1']
predictionsP
test$Output<-as.factor(test$Output)
test$Output

# Calculate MCC
library(caret)  # Make sure the 'caret' package is loaded
#mcc_value <- mcc(test$Output, predicted_class)
#mcc_value
#mcc(test$Output, results)
#round(mcc(test$Output, results), 4)
#results
#test$Output
library(pROC)

roc_curve <- roc(test$Output, predictionsP)
auc_value <- auc(roc_curve)
print(paste("SVM AUC:", auc_value))
library(caret)
confusionMatrix(predictions, test$Output, positive = '1', mode = "everything")

# Assuming 'results' contains your predicted values and 'test' is your test dataset
preds <- predictions
preds
actuals <- test$Output
actuals

# Calculate MCC
confusion_matrix <- table(Actual = actuals, Predicted = preds)
confusion_matrix
tp <- confusion_matrix["1","1"]  # True Positives
tn <- confusion_matrix["-1","-1"]  # True Negatives
fp <- confusion_matrix["-1","1"]  # False Positives
fn <- confusion_matrix["1","-1"]  # False Negatives

mcc <- (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))

# Print MCC
print(mcc)
library(pROC)
TPR <- tp / (tp + fn)
TPR
FPR <- fp / (fp + tn)
FPR

TPR_vector <- c(0, TPR, 1)  # Include (0,0) and (1,1) points for ROC curve
FPR_vector <- c(0, FPR, 1)
TPR_vector
# Calculate the AUC by numerical integration
auc_value <- sum(diff(FPR_vector) * TPR_vector[-1])

# Print the AUC value
cat("AUC Value:", auc_value, "\n")
confusion_matrix <- confusionMatrix(predictions,  test$Output)
confusion_matrix

draw_confusion_matrix <- function(cm) {
  
  layout(matrix(1))
  par(mar=c(2,2,2,2))
  #plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  plot(c(123, 345), c(300, 452), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  #title('CONFUSION MATRIX', cex.main=2)
  #title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#AF97D0')
  text(195, 435, '-1', cex=1.2)
  rect(250, 430, 340, 370, col='#A7AD50')
  text(295, 435, '1', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#A7AD50')
  rect(250, 305, 340, 365, col='#AF97D0')
  text(140, 400, '-1', cex=1.2, srt=90)
  text(140, 335, '1', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='black')
  text(195, 335, res[2], cex=1.6, font=2, col='black')
  text(295, 400, res[3], cex=1.6, font=2, col='black')
  text(295, 335, res[4], cex=1.6, font=2, col='black')
  
  
}  
draw_confusion_matrix(confusion_matrix)









