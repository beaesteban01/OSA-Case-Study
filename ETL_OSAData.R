########################################
# OSA Use Case
#
# Simple ETL process on a single Excel file
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




Input_file <- "Info_BDApnea_QuironMalaga.xlsx"
Output_file <- "OSA_DB_UPM.xlsx"

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

# EXPLORATORY DATA ANALYSIS
# See what kind of data we have in the df
vis_dat(df_tmp1) 
str(df_tmp1)

color <- brewer.pal(7, "Set3") 
#length nos da el value contando los NA, en realidad es -1 de eso 


barplot(table(df_tmp1$Illness), ylim = c(0,550), main= "Illness", col=color)
length(unique(df_tmp1$Illness)) # 247 diff. values

length(unique(df_tmp1$Gender)) # 2 diff. values
count(df_tmp1,df_tmp1$Gender)
barplot(table(df_tmp1$Gender), ylim = c(0,550), main= "Genders", col=color)

length(unique(df_tmp1$Smoker)) # 6 diff. values
count(df_tmp1,df_tmp1$Smoker)
barplot(table(df_tmp1$Smoker), ylim = c(0,550),main= "Smokers", col=color)

length(unique(df_tmp1$Snorer)) # 8 diff. values
count(df_tmp1,df_tmp1$Snorer)
barplot(table(df_tmp1$Snorer), ylim = c(0,550),main= "Snorer", col=color)

length(unique(df_tmp1$Patient)) # 684 diff. values
length(unique(df_tmp1$Weight)) # 92 diff. values



# FEATURES TRANSFORMATIONS

# Transformation of illness into si-no
df_tmp1$Illness[which(df_tmp1$Illness != "no")] <- "si"
count(df_tmp1,df_tmp1$Illness)

# Factor and numeric conversion 
par(mfrow=c(1,2))
df_tmp1$Gender = factor(df_tmp1$Gender)
str(df_tmp1$Gender)
barplot(table(df_tmp1$Gender))
df_tmp1$Gender = as.numeric(df_tmp1$Gender)
barplot(table(df_tmp1$Gender))

par(mfrow=c(1,1))
par(mfrow=c(1,2))
df_tmp1$Smoker = factor(df_tmp1$Smoker)
str(df_tmp1$Smoker)
barplot(table(df_tmp1$Smoker))
df_tmp1$Smoker = as.numeric(df_tmp1$Smoker)
barplot(table(df_tmp1$Smoker))

par(mfrow=c(1,1))
par(mfrow=c(1,2))
df_tmp1$Illness = factor(df_tmp1$Illness)
str(df_tmp1$Illness)
barplot(table(df_tmp1$Illness))
df_tmp1$Illness = as.numeric(df_tmp1$Illness)
barplot(table(df_tmp1$Illness))

par(mfrow=c(1,1))
par(mfrow=c(1,2))

df_tmp1$Snorer = factor(df_tmp1$Snorer)
summary(df_tmp1$Snorer)
barplot(table(df_tmp1$Snorer))
df_tmp1$Snorer= as.numeric(df_tmp1$Snorer)
barplot(table(df_tmp1$Snorer))

# Numeric conversion
# String values replace with NA
df_tmp1$Weight = as.numeric(df_tmp1$Weight)

vis_dat(df_tmp1)

# Now change -1 values for NA in all columns (i.e. variables)
df_tmp2 <- replace_with_na_all(df_tmp1,condition = ~.x == -1)

vis_dat(df_tmp2)

# Finally remove (drop out) all rows containing a NA
# at least one column

df_final <- df_tmp2 %>% drop_na()
# %>% para aplicar una operacion sobre el df


vis_dat(df_final) 
str(df_final) #REcompruebo que menos patient y gender es numeric

######### SAVING CLEAN DATA ##################################

write_xlsx(df_final,
           paste(Data_Directory, Output_file, sep = ""))

