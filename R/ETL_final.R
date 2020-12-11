########################################
# Definition of final dataset 
# after EDA

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




Input_file <- "Info_BDApnea_QuironMalaga.xlsx"
Output_file <- "OSA_DB_final.xlsx"

Data_Directory <- "/Users/beatrizesteban/OneDrive - Universidad Politécnica de Madrid/Segundo MUIT/PRDL&MLLB/OSA/DATA/"


# paste --> concatenation of to strings
df_tmp <- read_excel(paste(Data_Directory, Input_file, sep = ""))

typeof(df_tmp)
is.data.frame(df_tmp)

### NOTE: ##############################################
# df_tmp is NOT only a data frame!
# use as.data.frame to avoid later problems
class(df_tmp)
df_tmp = as.data.frame(df_tmp)
class(df_tmp)

######## PREPARING THE DATA FRAME 
# Select only those columns (variables) we are going
# to work with:
# Patient, Gender, IAH, Peso, Talla, Edad, PerCervical


df_tmp1 <- select(df_tmp, Patient, Gender, IAH, Peso, Talla, Edad, PerCervical, Fumador, Roncador, Enfermedades)
df_tmp1 <- df_tmp1 %>% rename(Weight = Peso,
                              Height = Talla,
                              Age = Edad,
                              Cervical = PerCervical, 
                              Smoker = Fumador, 
                              Snorer = Roncador, 
                              Illness = Enfermedades)

# FEATURES TRANSFORMATIONS

# Transformation of illness into si-no
df_tmp1$Illness[which(df_tmp1$Illness != "no")] <- "si"

# Delete Snorer and Smoker
df_tmp1 <- select(df_tmp1, - Smoker, - Snorer)

# Add BMI column
df_tmp1$BMI <- with(df_tmp1, Weight / (Height/100.0)^2)

# Factor and numeric conversion 

df_tmp1$Gender = factor(df_tmp1$Gender)
df_tmp1$Gender = as.numeric(df_tmp1$Gender)

df_tmp1$Smoker = factor(df_tmp1$Smoker)
df_tmp1$Smoker = as.numeric(df_tmp1$Smoker)

df_tmp1$Illness = factor(df_tmp1$Illness)
df_tmp1$Illness = as.numeric(df_tmp1$Illness)

df_tmp1$Snorer = factor(df_tmp1$Snorer)
df_tmp1$Snorer= as.numeric(df_tmp1$Snorer)

# Numeric conversion
# String values replace with NA
df_tmp1$Weight = as.numeric(df_tmp1$Weight)

# Now change -1 values for NA in all columns (i.e. variables)
df_tmp2 <- replace_with_na_all(df_tmp1,condition = ~.x == -1)

# Finally remove (drop out) all rows containing a NA
# at least one column

df_final <- df_tmp2 %>% drop_na()
# %>% para aplicar una operacion sobre el df


vis_dat(df_final) 
str(df_final) #REcompruebo que menos patient y gender es numeric

######### SAVING CLEAN DATA ##################################

write_xlsx(df_final,
           paste(Data_Directory, Output_file, sep = ""))

