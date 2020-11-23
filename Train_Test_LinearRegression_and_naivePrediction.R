######################################################
###
### Discussion on training / test (validation) Data
###
### Linear Regression and Naive prediction
###
######################################################


rm(list = ls())

library(readxl)
library(dplyr)


Input_file <- "OSA_DB_UPM.xlsx"

Data_Directory <- "D:\\OSA_CaseStudy\\DATA\\"

df_OSA <- read_excel(paste(Data_Directory, Input_file, sep = ""))
df_OSA = as.data.frame(df_OSA)
names(df_OSA)
dim(df_OSA)

df_OSA$Gender = factor(df_OSA$Gender)

attach(df_OSA)
Naive_IAH = mean(IAH) #media sin hacer nada

print(paste("Mean IAH: ", Naive_IAH))


## R^2
1 - (sum((IAH - Naive_IAH)^2) / sum((IAH - mean(IAH))^2)) 

## MAE
mae_Naive <- mean(abs(IAH - Naive_IAH))

print(paste("MAE Naive Predictor: ", mae_Naive))


###################################################
# ------USING ALL THE DATA----- #

# Linear Regression

lm.model <- lm(IAH ~ Weight + Cervical + Gender)

IAH_predictions = predict(lm.model,df_OSA)

mae_Predict <- mean(abs(IAH - IAH_predictions))

print(paste("MAE Naive    Predictor: ", mae_Naive))
print(paste("MAE lm.model Predictor: ", mae_Predict))




###################################################
# ------USING TRAIN / TEST DATA----- #


set.seed(123)

library(caTools)  # If error: install it using Packages Install

split_series = sample.split(df_OSA,SplitRatio=0.8)
df_OSA.train=subset(df_OSA, split_series==TRUE)
df_OSA.test=subset(df_OSA, split_series==FALSE)

lm.model.train <- lm(IAH ~ Weight + Cervical + Gender, data = df_OSA.train)

IAH_predictions_train = predict(lm.model.train,df_OSA.train)
mae_Predict_train <- mean(abs(df_OSA.train$IAH - IAH_predictions_train))

IAH_predictions_test = predict(lm.model.train,df_OSA.test)
mae_Predict_test <- mean(abs(df_OSA.test$IAH - IAH_predictions_test))

mae_Naive_test <- mean(abs(df_OSA.test$IAH - mean(df_OSA.train$IAH)))

print(paste("MAE Naive TEST   Predictor: ", mae_Naive_test))
print(paste("MAE lm.model TRAIN Predictor: ", mae_Predict_train))
print(paste("MAE lm.model TEST  Predictor: ", mae_Predict_test))

