#x<-"D:\\Research_Work\\Disertation_Project_2\\Raw Data\\featureExtraction\\reviewer_response\\LinearSVC\\BIN\\selected_training_merged_file.csv"
#y<-"D:\\Research_Work\\Disertation_Project_2\\Raw Data\\featureExtraction\\reviewer_response\\LinearSVC\\BIN\\selected_validation_merged_file.csv"
#x<-"D:\\Research_Work\\Disertation_Project_2\\Raw Data\\featureExtraction\\reviewer_response\\LinearSVC\\PSSM\\selected_training_merged_file.csv"
#y<-"D:\\Research_Work\\Disertation_Project_2\\Raw Data\\featureExtraction\\reviewer_response\\LinearSVC\\PSSM\\selected_validation_merged_file.csv"

x<-"D:\\Research_Work\\Disertation_Project_2\\Raw Data\\featureExtraction\\reviewer_response\\test_result\\ADT\\selected_training_merged_file.csv"
y<-"D:\\Research_Work\\Disertation_Project_2\\Raw Data\\featureExtraction\\reviewer_response\\test_result\\ADT\\selected_validation_merged_file.csv"
#x<-"D:\\Research_Work\\Disertation_Project_2\\Raw Data\\featureExtraction\\reviewer_response\\GA\\PSSM\\selected_training_merged_file.csv"
#y<-"D:\\Research_Work\\Disertation_Project_2\\Raw Data\\featureExtraction\\reviewer_response\\GA\\PSSM\\selected_validation_merged_file.csv"

library(e1071)
library(caret)
library(rpart)
library(ROCR)
library(mltools)
library(pROC)



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


# Assuming "Output" is your target variable and "train" is your training dataset
tuneGrid <- expand.grid(cp = seq(0.01, 0.1, by = 0.01))  # Define a grid of cp values to tune


fitControl <- trainControl(method = "cv", number = 5)  # Define cross-validation settings

tmodel <- train(Output ~ ., data = train, method = "rpart", tuneGrid = tuneGrid, trControl = fitControl)

# To view the best hyperparameters
print(tmodel)

# To access the best model with the tuned hyperparameters
mymodel2 <- tmodel$finalModel
mymodel2
#
#scale(test$pseudo_10)
#setdiff(colnames(train),colnames(test))
#results<-predict(tmodel2, test)
#ind <- colSums(is.na(test)) == nrow(test)
#ind
#pos<-which(ind==TRUE)
#test[,pos]<-data_other_backup[,pos]
#names(test)[ind]

# Predict values for the test dataset
results <- predict(mymodel2, newdata = test)

# 'results' now contains the predicted values
results
resultsA<-results[, "1"]

roc_curve <- roc(test$Output, resultsA)
auc_value <- auc(roc_curve)
print(paste("SVM AUC:", auc_value))
# Convert predicted probabilities in 'results' to class labels using a threshold
threshold <- 0.5  # You can adjust this threshold as needed
predicted_class <- ifelse(results[, "1"] > threshold, 1, -1)
predicted_class
test$Output<-as.factor(test$Output)
test$Output
# Now, 'predicted_class' contains the predicted class labels based on the threshold
predicted_factor <- factor(predicted_class, levels = levels(test$Output))
predicted_factor
#test$Output<-as.factor(test$Output)
#test$Output
#predict(mymodel2,test)
# Assuming 'results' contains your predicted values and 'test' is your test dataset

# Convert 'test$Output' factor to numeric values
#actuals_numeric <- as.numeric(test$Output)

# Convert 'predicted_class' factor to numeric values
#predicted_numeric <- as.numeric(predicted_class)
#length(actuals_numeric)
#length(predicted_numeric)
# Calculate MCC

library(caret)  # Make sure the 'caret' package is loaded





library(plyr)
data_other_backup$Output<-as.factor(data_other_backup$Output)



test$Output
predicted_factor
# Create a confusion matrix
library(caret)
confusionMatrix(predicted_factor, test$Output, positive = '1', mode = "everything")

# Assuming 'results' contains your predicted values and 'test' is your test dataset
preds <- predicted_factor
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

confusion_matrix <- confusionMatrix(predicted_factor,  test$Output)
confusion_matrix

draw_confusion_matrix <- function(cm) {
  
  layout(matrix(1))
  par(mar=c(2,2,2,2))
  #plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  plot(c(123, 345), c(300, 452), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
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





