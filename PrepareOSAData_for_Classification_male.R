########################################
# OSA data analysis using:
#         - Classification models
#
# We will try to classify extreme OSA cases
#   IAH <= 10 vs IAH >=30
#

rm(list=ls())
Input_file <- "OSA_DB_UPM.xlsx"
Output_file <- "OSA_class.xlsx"
Output_file_bin <- "OSA_class_bin.xlsx"
Data_Directory <- "/Users/beatrizesteban/OneDrive - Universidad Politécnica de Madrid/Segundo MUIT/PRDL&MLLB/OSA/DATA/"



# Using readxl package to read an Excel file
# Install the readxl package is nor already installed

library(readxl)

df_OSA <- read_excel(paste(Data_Directory, Input_file, sep = ""))

df_OSA = as.data.frame(df_OSA)
names(df_OSA)
dim(df_OSA)

## We will begin considering male subjets
### Male population
#df_OSA_male=subset(df_OSA, Gender==1)


########################################
# We add column to tag three clases:
#     Healthy (IAH <= 10)
#     Mild (10<IAH<30)
#     Severe (IAH >=30)

# We will use dplyr library (mutate operator)
library(dplyr)

#3 classes
df_OSA <- df_OSA %>%
          mutate(OSA = ifelse(IAH <= 10, "Healthy",
          ifelse(IAH>=30, "Severe", "Mild")))
# 2 clasess
df_OSA_bin <- df_OSA %>% filter(OSA != "Mild")


# Define OSA as a factor!
  
df_OSA$OSA = factor(df_OSA$OSA)
df_OSA_bin$OSA = factor(df_OSA_bin$OSA)

# Add BMI column
df_OSA$BMI <-
  with(df_OSA, Weight / (Height/100.0)^2)
df_OSA_bin$BMI <-
  with(df_OSA_bin, Weight / (Height/100.0)^2)

summary(df_OSA_bin)
summary(df_OSA)


###############################################
### READ MORE: On using R as SQL
###
# https://blog.exploratory.io/using-dplyr-to-query-databases-directly-instead-of-using-sql-fed43f059ed6

#df_OSA_male %>% group_by(OSA) %>% summarise(Ave_IAH = mean(IAH))

###### SAVE the data frame into an EXCEL file
#
# Output_file <- "OSA_extreme_male.xlsx"
#
#

library(writexl)
#guardo el de 3 clases
write_xlsx(df_OSA,
           paste(Data_Directory, Output_file, sep = ""))
#guardo el de 2 clases
write_xlsx(df_OSA_bin,
           paste(Data_Directory, Output_file_bin, sep = ""))

