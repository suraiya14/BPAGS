####################################################################
## Script to select best variables for a classification mode using genetic algorithms. 
## Based on `GA` library with custom fitness function. 
## This script is explained in the post: 
## Contact: https://twitter.com/pabloc_ds
####################################################################

# Install packages if missing
list.of.packages <- c("parallel", "doParallel", "caret", "randomForest", "funModeling", "tidyverse", "GA")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load libraries
library(caret)
library(randomForest)
library(funModeling)
library(tidyverse)
library(GA)
library(dplyr)

library(doMC)
#detectCores()
registerDoMC(cores = 10)

data(mdrr)
mdrrDescr

source("D:\\Research Work\\Disertation Project 2/R script/lib_ga.R")

data=read.csv("D:\\Research Work\\Disertation Project 2\\Raw Data\\featureExtraction\\PearsonCorrFeatureSetAddNewSeq.csv", header = TRUE)  
nrow(data)
# Data preparation
data2=na.omit(data) # <- use with care...

df<-data.frame(data2)
df
df$Output[df$Output==1]<-"Yes"
df$Output[df$Output==-1]<-"No"
df$Output

data2


data_y=as.factor(df$Output)
data_x=select(df, -Output)

data_y
data_x

# GA parameters
param_nBits=ncol(data_x)
col_names=colnames(data_x)

col_names

# Executing the GA 
# Executing the GA 
ga_GA_1 = ga(fitness = function(vars) custom_fitness(vars = vars, 
                                                     data_x =  data_x, 
                                                     data_y = data_y, 
                                                     p_sampling = 0.7), # custom fitness function
             type = "binary", # optimization data type
             crossover=gabin_uCrossover,  # cross-over method
             elitism = 3, # number of best ind. to pass to next iteration
             pmutation = 0.03, # mutation rate prob
             popSize = 50, # the number of indivduals/solutions
             nBits = param_nBits, # total number of variables
             names=col_names, # variable name
             run=5, # max iter without improvement (stopping criteria)
             maxiter = 50, # total runs or generations
             monitor=plot, # plot the result at each iteration
             keepBest = TRUE, # keep the best solution at the end
             parallel = T, # allow parallel procesing
             seed=84211 # for reproducibility purposes
)

ga_GA_1
# Checking the results
summary(ga_GA_1)

# Following line will return the variable names of the final and best solution
best_vars_ga=col_names[ga_GA_1@solution[1,]==1]

# Checking the variables of the best solution...
best_vars_ga

write.csv(best_vars_ga, file = "D:\\Research Work\\Disertation Project 2\\Raw Data\\featureExtraction\\GAFeatureSetRFAddNewseq.csv", row.names = FALSE)


df<- data.frame(best_vars_ga)

length(df)
df
df[nrow(df) + 1,] <- c("Output")
#add row
#final_df[nrow(final_df) + 1,] <- c("Output")
#print
print(df)

write.csv(df, "D:\\Research Work\\Disertation Project 2\\Raw Data\\featureExtraction\\GA/addNewSeq/RF/GA_Generate_featRF.csv", row.names = FALSE)

file1<-read.csv("D:\\Research Work\\Disertation Project 2\\Raw Data\\featureExtraction\\GA/addNewSeq/RF/GA_Generate_featRF.csv", header = TRUE)
file1

final_df <- as.data.frame(t(file1))
final_df<-data.frame(final_df)

final_df
write.csv(final_df, "D:\\Research Work\\Disertation Project 2\\Raw Data\\featureExtraction\\GA/addNewSeq/RF/GA_Generate_featRFNew.csv", row.names = FALSE)

#file<-read.csv(file = "D:\\Research Work\\Disertation Project 2\\Raw Data\\featureExtraction\\GA/RF/addNewSeq/RF/GA_Generate_featRFNew.csGA_Generate_featRFNew.csv", header=TRUE)

