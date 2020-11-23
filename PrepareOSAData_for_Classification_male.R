########################################
# OSA data analysis using:
#         - Classification models
#
# We will try to classify extreme OSA cases
#   IAH <= 10 vs IAH >=30
#

rm(list=ls())
Input_file <- "OSA_DB_UPM.xlsx"
Output_file <- "OSA_extreme_male.xlsx"
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
df_OSA_male=subset(df_OSA, Gender==1)


########################################
# We add column to tag three clases:
#     Healthy (IAH <= 10)
#     Mild (10<IAH<30)
#     Severe (IAH >=30)

# We will use dplyr library (mutate operator)
library(dplyr)

df_OSA_male <- df_OSA_male %>%
          mutate(OSA = ifelse(IAH <= 10, "Healthy",
          ifelse(IAH>=30, "Severe", "Mild")))

df_OSA_male <- df_OSA_male %>% filter(OSA != "Mild")


# Define OSA as a factor!
  
df_OSA_male$OSA = factor(df_OSA_male$OSA)

# Add BMI column
df_OSA_male$BMI <-
  with(df_OSA_male, Weight / (Height/100.0)^2)

summary(df_OSA_male)

###############################################
### READ MORE: On using R as SQL
###
# https://blog.exploratory.io/using-dplyr-to-query-databases-directly-instead-of-using-sql-fed43f059ed6

df_OSA_male %>% group_by(OSA) %>% summarise(Ave_IAH = mean(IAH))

###### SAVE the data frame into an EXCEL file
#
# Output_file <- "OSA_extreme_male.xlsx"
#
#

library(writexl)

write_xlsx(df_OSA_male,
           paste(Data_Directory, Output_file, sep = ""))

