########################################
# OSA data analysis using:
#         - linear regression models
#

Input_file <- "OSA_DB_UPM.xlsx"

Data_Directory <- "/Users/beatrizesteban/OneDrive - Universidad Politécnica de Madrid/Segundo MUIT/PRDL&MLLB/OSA/DATA/"


# Using readxl package to read an Excel file
# Install the readxl package is nor already installed

library(readxl)

df_OSA <- read_excel(paste(Data_Directory, Input_file, sep = ""))

df_OSA = as.data.frame(df_OSA)
names(df_OSA)
dim(df_OSA)

## Describe the Database

# First define Gender as a factor!

#df_OSA$Gender = factor(df_OSA$Gender)
summary(df_OSA)
attach(df_OSA)
plot(Smoker, Snorer, )

### see distributions (histogram) profe en clase
hist(df_OSA$IAH) #poison distribution, why? to measure nº of time somt happens but in regression we supose that distributions are gaussian
#solution--> coger el log (+1 por asegurar)
hist(log(df_OSA$IAH+1)) #but why IAH is pisson distribution=? these type of question for data anaalysis

hist(df_OSA$Weight)

# typical: describe info --> groupby 
# ej: vemos que hay mas male que female por lo que separar en estos grupos es relevante

# NECESITA ALgo antes: describeBy(df_OSA, group='Gender')

#attach is like to put in the path the variables the df
attach(df_OSA)
plot(Age,IAH) #mirar que sacamos de este plot. eJ: los dos que estan arribaa aaislados, que significan?
# vas a esos dos pacientes y miras sus caracteristicas
# check one case: 
df_OSA[579,] #--> descubres que es joven pero gordo, por eso tiene IAH tan alto



# See relations between variables
attach(df_OSA)
pairs(~ IAH + Gender + Weight + Height + Cervical + Age)

## PLOT Correlation Matrix

# FIRST
# install corrplot and then load it
library(corrplot)
# back to as.numeric for including it..

df_OSA_C=df_OSA

#df_OSA_C$Gender = as.numeric(df_OSA_C$Gender) 
#patient no lo usa para nada
M <- cor(subset(df_OSA_C, select = - Patient))
corrplot(M, method="number")
corrplot(M, method="circle")
####stop here 29 sept

## 30 sept
# simple variable: cervical and IAH
lm.fit= lm(IAH ~ Cervical)
plot(Cervical, IAH)
abline(lm.fit)
summary(lm.fit)

#residuals: comapration of predicted and true values (???)
# coeffs: intercep = b1 and cervical = b0 
# gives as some metrics: residual, R sqare, f-statistics

# there is no strong connection betwen IAH and cervical --> vemos en el plot
# and so this is a difficult case because there are not strong relations in general

# Study the use of Simple and Multiple LR models
# multiple variables: more than cervical and iah
# witgh and cervical are very correlated (ver en alguna matrix que no me ha salido) -->
# some parameters are more important than others

lm.fit=lm(IAH~Weight+Height+Cervical+Age+Gender)
# think what happends qehn adding a categorical variable (gender) --> algo mal 
# ver pptx ej pg 64
summary(lm.fit)

# make predictions#
predIAH <- predict(lm.fit, df_OSA[c('Weight', 'Height', 'Cervical', 'Age', 'Gender')]) 
#--> captura

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

