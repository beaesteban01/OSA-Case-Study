########################################
# OSA data analysis using:
#         - linear regression models
#

# Clear the working space
rm(list=ls())


library(readxl) # Using readxl package to read an Excel file
library(writexl) # write excelfile

library(dplyr) # To work with datasets, basic library
library(tidyr) # easy to clean the daata an dowrk with. Each column is a variable and each row and obs

library(visdat) #visualize 
library("RColorBrewer") #For colors

library(naniar) # for missing data
# Write the clean data into Output_file
# you can install writexl package
library(corrplot) # correlation plots

#Input_file <- "Info_BDApnea_QuironMalaga.xlsx"
Input_file <- "OSA_DB_UPM.xlsx"

Data_Directory <- "/Users/beatrizesteban/OneDrive - Universidad Politécnica de Madrid/Segundo MUIT/PRDL&MLLB/OSA/DATA/"

df_OSA <- read_excel(paste(Data_Directory, Input_file, sep = ""))

df_OSA = as.data.frame(df_OSA)
names(df_OSA)
dim(df_OSA)

# COLORS
color <- brewer.pal(7, "Set3") 
greenDegradado <- brewer.pal(7, "Greens")


# DISTRIBUTION HISTOGRAM
hist(df_OSA$IAH, main= 'IAH Histogram', xlab = "IAH", col= color) #poison distribution, why? to measure nº of time somt happens but in regression we supose that distributions are gaussian
#solution--> coger el log (+1 por asegurar)
hist(log(df_OSA$IAH+1), main= 'log(IAH+1) Histogram', xlab = "IAH", col= color) #but why IAH is pisson distribution=? these type of question for data anaalysis

hist(df_OSA$Height, main= 'Height Histogram', xlab = "Height", col= color)
hist(df_OSA$Weight, main= 'Weight Histogram', xlab = "Weight", col= color)
hist(df_OSA$Cervical, main= 'Cervical Histogram', xlab = "Cervical", col= color)
hist(df_OSA$Age, main= 'Age Histogram', xlab = "Age", col= color)
hist(df_OSA_feat2$BMI, main= 'BMI Histogram', xlab = "BMI", col= color)
#hist(df_OSA$Illness, main= 'Illness Histogram', xlab = "Illness", col= color)
par(mfrow=c(1,2))
hist(df_OSA$Height[df_OSA$Gender==1], main= 'Male Height Histogram', xlab = "Height", col= color)
hist(df_OSA$Height[df_OSA$Gender==2], main= 'Female Height Histogram', xlab = "Height", col= color)

hist(df_OSA$IAH[df_OSA$Gender==1], main= 'Male IAH Histogram', xlab = "IAH", col= color)
hist(df_OSA$IAH[df_OSA$Gender==2], main= 'Female IAH Histogram', xlab = "IAH", col= color)

hist(log(df_OSA$IAH+1)[df_OSA$Gender==1], main= 'Male log(IAH+1) Histogram', xlab = "IAH", col= color)
hist(log(df_OSA$IAH+1)[df_OSA$Gender==2], main= 'Female log(IAH+1) Histogram', xlab = "IAH", col= color)

hist(log(df_OSA$IAH+1)[df_OSA$Illness==1], main= 'No Illness vs IAH Histogram', xlab = "IAH", col= color)
hist(log(df_OSA$IAH+1)[df_OSA$Illness==2], main= 'Illness vs IAH Histogram', xlab = "IAH", col= color)

# typical: describe info --> groupby 
# ej: vemos que hay mas male que female por lo que separar en estos grupos es relevante

# NECESITA ALgo antes: describeBy(df_OSA, group='Gender')

#attach is like to put in the path the variables the df
# attach(df_OSA)
# plot(Age,IAH) #mirar que sacamos de este plot. eJ: los dos que estan arribaa aaislados, que significan?
# # vas a esos dos pacientes y miras sus caracteristicas
# # check one case: 
# df_OSA[579,] #--> descubres que es joven pero gordo, por eso tiene IAH tan alto



# See relations between variables
attach(df_OSA)
pairs(~ IAH + Gender + Weight + Height + Cervical + Age + Smoker + Snorer+Illness)

## PLOT Correlation Matrix

# FIRST
# install corrplot and then load it

# back to as.numeric for including it..

df_OSA_C=df_OSA

#df_OSA_C$Gender = as.numeric(df_OSA_C$Gender) 
#patient no lo usa para nada
M <- cor(subset(df_OSA_C, select = -Patient))

corrplot(M, method="number")
corrplot(M, method="circle", col=greenDegradado, tl.col = "black")

par(mfrow=c(1,1))
# SIMPLE Regression
lm.fit= lm(IAH ~ Cervical)
plot(Cervical, IAH)
abline(lm.fit)
summary(lm.fit)

lm.fit= lm(Weight ~ Cervical)
plot(Cervical, Weight)
abline(lm.fit)
summary(lm.fit)

lm.fit= lm(Weight ~ IAH)
plot(IAH, Weight)
abline(lm.fit)
summary(lm.fit)

lm.fit= lm(IAH ~ Age)
plot(Age, IAH)
abline(lm.fit)
summary(lm.fit)


#residuals: comapration of predicted and true values (???)
# coeffs: intercep = b1 and cervical = b0 
# gives as some metrics: residual, R sqare, f-statistics

# there is no strong connection betwen IAH and cervical --> vemos en el plot
# and so this is a difficult case because there are not strong relations in general

# MULTIPLE: more than cervical and iah
# witgh and cervical are very correlated (ver en alguna matrix que no me ha salido) -->
# some parameters are more important than others

# lm.fit=lm(IAH~Weight+Height+Cervical+Age+Gender)
# # think what happends qehn adding a categorical variable (gender) --> algo mal 
# # ver pptx ej pg 64
# #plot(IAH~Weight+Height+Cervical+Age+Gender+BMI)
# summary(lm.fit)
# 
# # make predictions
# predIAH <- predict(lm.fit, df_OSA[c('Weight', 'Height', 'Cervical', 'Age', 'Gender')]) 
# #predIAH <- predict(lm.fit, df_OSA_C[c('Weight', 'Height', 'Cervical', 'Age', 'Gender', 'BMI')]) 
# plot(IAH, predIAH)


# FEATURE ENGINEERING

# Delete smoker and snorer
df_OSA_feat1 <- select(df_OSA_C,- Smoker, -Snorer)
str(df_OSA_feat1)
# Creation of new column
df_OSA_feat2 <- mutate(df_OSA_feat1, BMI = ((Weight)/(Height*10^-2)^2))
str(df_OSA_feat2)

attach(df_OSA_feat2)
M <- cor(subset(df_OSA_feat2, select = -Patient))

# SIMPLE regression
lm.fit= lm(BMI ~ IAH)
plot(IAH, BMI)
abline(lm.fit)
summary(lm.fit)

#MULTIPLE 1- weigh and cervical
lm.fit1=lm(IAH~Weight+Cervical)
summary(lm.fit1)
plot(lm.fit1, main = 'Multiple Regression 1')

#MULTIPLE 2- weigh and cervical and BMI
lm.fit2=lm(IAH~Weight+Cervical+BMI)
summary(lm.fit2)
plot(lm.fit2, main = 'Multiple Regression 2')

M <- cor(subset(df_OSA_feat2, select = -Patient))
corrplot(M, method="number")

# MULTIPLE 3 - todo
# witgh and cervical are very correlated (ver en alguna matrix que no me ha salido) -->
# some parameters are more important than others

lm.fit3=lm(IAH~Weight+Height+Cervical+Age+Gender+BMI)
# think what happends qehn adding a categorical variable (gender) --> algo mal 
# ver pptx ej pg 64

summary(lm.fit3)
plot(lm.fit3, main = 'Multiple Regression 3')

#MULTIPLE 4  Todo - Gender
lm.fit4=lm(IAH~Weight+Cervical+Height+Age +BMI)
summary(lm.fit4)
plot(lm.fit4, main = 'Multiple Regression 4')

#MULTIPLE 5 todo - Gender -BMI
lm.fit5=lm(IAH~Weight+Cervical+Height+Age)
summary(lm.fit5)
plot(lm.fit5, main = 'Multiple Regression 5')

#MULTIPLE 6 todo - bmi sin heigh and weight
lm.fit6=lm(IAH~Cervical+BMI+Age)
summary(lm.fit6)
plot(lm.fit6, main = 'Multiple Regression 6')




# PREDICTIONS
predIAH <- predict(lm.fit, df_OSA_feat2[c('Weight', 'Height', 'Cervical', 'Age', 'Gender', 'BMI')]) 
plot(IAH, predIAH, ylab = 'Predicted IAH')

plot(predict(lm.fit),residuals(lm.fit),xlab='Fitted values',ylab='Residuals')


###  NO HECHO
# Study independently male and female populations: SUGGESTION

### Male population
df_OSA_male=subset(df_OSA_C, Gender==1)

# Another way
# df_OSA_male = df_OSA_C[df_OSA_C$Gender == 1, ]

names(df_OSA_male)
attach(df_OSA_male)

lm_male.fit=lm(IAH~Height+Cervical+Age+Weight)

summary(lm_male.fit)

############ Female population ################

df_OSA_female=subset(df_OSA_C, Gender==2)

# Another way
# df_OSA_female = df_OSA_C[df_OSA_C$Gender == 2, ]

names(df_OSA_female)
attach(df_OSA_female)

lm_female.fit=lm(IAH~Height+Cervical+Age+Weight)

summary(lm_female.fit)

# BMI add a column
df_OSA_C$BMI <- with(df_OSA_C, Weight / (Height/100.0)^2)

attach(df_OSA_C)

lm.fit=lm(IAH~BMI+Cervical+Age)

summary(lm.fit)


########################################
########################################
####   NEXT STEPS:
####
####  You can try:
####        - Regularization
####        - Feature selection
####        - other Regression models
####
####  BUT you can wait to see Classification Script
####           and use regression with some libraries as CARET
####


####

