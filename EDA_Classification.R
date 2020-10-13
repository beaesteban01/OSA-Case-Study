##########################################################
####### BEFORE TESTING DIFFERENT Classification Models
#         to classify extreme OSA cases
#               IAH <= 10 vs IAH >=30
#######
#######    try some EDA (Exploratory Data Analysis)
#

rm(list=ls())

########################################
#
#         - load the data from

Input_file <- "OSA_extreme_male.xlsx"

Data_Directory <- "D:\\OSA_CaseStudy\\DATA\\"

# Using readxl package to read an Excel file
# Install the readxl package is nor already installed

library(readxl)

df_OSA_male <- read_excel(paste(Data_Directory, Input_file, sep = ""))

summary(df_OSA_male)

# Define OSA column as a factor for being used be
# classification models
df_OSA_male$OSA = factor(df_OSA_male$OSA)

########################################################
### For example: the correlation among predictors and IAH

cor(df_OSA_male[,3:7])

# for examle a visualization
library(corrplot)

correlations = cor(df_OSA_male[,3:7])
corrplot(correlations, method="number")


############################################
#### you can use ggplot2 for plotting
#### histograms of a dataframe by group

# set the plotting area into a 1*2 array
par(mfrow=c(1,2))

hist(subset(df_OSA_male, OSA=="Healthy")$IAH)
hist(subset(df_OSA_male, OSA=="Severe")$IAH)

# set the plotting area into a 1*2 array
par(mfrow=c(1,2))

hist(subset(df_OSA_male, OSA=="Healthy")$Cervical)
hist(subset(df_OSA_male, OSA=="Severe")$Cervical)



############################################
##
## Some Plots and scatter plots per class
## using lattice (see ?lattice):
#         lattice add-on package is a powerful
#         and elegant high-level data
#         visualization system with an
#         emphasis on multivariate da

library(lattice)

# Each group in a separate mini plot
xyplot(BMI ~ Age | OSA, data = df_OSA_male)

xyplot(BMI ~ Age , 
       groups =  OSA, data = df_OSA_male,
       auto.key = list(corner = c(1, 1), cex = 0.7))


#############################################
### We can plot HISTOGRAMS by OSA Groups
### to explore they DISCRIMINATIVE power

################################################
###    ggplot2 
###       One of he best
###       R packages dedicated to data visualization

library(ggplot2)

ggplot(df_OSA_male, aes(x = BMI)) +
  geom_histogram(aes(color = OSA), fill = "white",
                 position = "identity", bins = 30, alpha = 0.1) +
  scale_color_manual(values = c("#00AF00", "#E7B800")) +
  scale_fill_manual(values = c("#00AF00", "#E7B800"))

### create a grid of plots (like subplot())

p1 <- ggplot(df_OSA_male, aes(x = BMI)) +
  geom_histogram(aes(color = OSA), fill = "white",
                 position = "identity", bins = 30, alpha = 0.1) +
  scale_color_manual(values = c("#00AF00", "#E7B800")) +
  scale_fill_manual(values = c("#00AF00", "#E7B800"))

p2 <- ggplot(df_OSA_male, aes(x = Height)) +
  geom_histogram(aes(color = OSA), fill = "white",
                 position = "identity", bins = 30, alpha = 0.1) +
  scale_color_manual(values = c("#00AF00", "#E7B800")) +
  scale_fill_manual(values = c("#00AF00", "#E7B800"))

library(gridExtra)
grid.arrange(p1, p2, ncol = 2)


### ... you can also use boxplots "by group"

par(mfrow=c(1,2))
attach(df_OSA_male)
boxplot(BMI ~ OSA)
boxplot(Height ~ OSA)

#### To have QUANTITATIVE information you can 
####    use some tests on the:
####     DISCRIMINATIVE POWER OF EACH FEATURE
####
### For example:

# http://www.r-tutor.com/elementary-statistics/non-parametric-methods/kruskal-wallis-test
# Kruskal-Wallis Test
# A collection of data samples are independent
# if they come from unrelated populations
# and the samples do not affect each other.
# Using the Kruskal-Wallis Test, we can decide
# whether the population distributions are identical
# without assuming them to follow the normal distribution. 


# The null hypothesis is that the BMI density are identical
# populations. To test the hypothesis, 

kruskal.test(BMI ~ OSA, data = df_OSA_male) 

kruskal.test(Height ~ OSA, data = df_OSA_male)

#### Please, understand and USE these or other tools
#### like this and
#### add your comments in your Half Term report

