########################################
# OSA Use Case
#
# Simple ETL process on a single Excel file
#

# Clear the working space
rm(list=ls())


library(readxl) # Using readxl package to read an Excel file
library(naniar)
# You can work with dplyr
# for using select(df_tmp, Patient, Gender,...)
# https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html
# dplyr's basic set of tools to apply on data frames

library(dplyr) # asi importamos librerias --> tipo import en python
#dplyr is a basic package for R tipo numpy, pandas etc para python
#data.table libreria de R que se usa para trabajar con datasets muy grandes

library(visdat) #visualize 
# You can use tidyr
# https://blog.rstudio.com/2014/07/22/introducing-tidyr/
# tidyr is a package that makes it easy to "tidy" 
# your data.
#
# Tidy data is data that's easy to work with:
# it's easy to munge (with dplyr), visualise (with ggplot2 or ggvis) and model (with R's hundreds of modelling packages). The two most important properties of tidy data are:
#
# Each column is a variable.
#
# Each row is an observation.

library(tidyr)
# Write the clean data into Output_file
# you can install writexl package

library(writexl)
library("RColorBrewer") #For colors

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

df_tmp1 <- select(df_tmp, Patient, Gender, IAH, Peso, Talla, Edad, PerCervical, Fumador, Roncador)
df_tmp1 <- df_tmp1 %>% rename(Weight = Peso,
                                Height = Talla,
                                Age = Edad,
                                Cervical = PerCervical, 
                                Smoker = Fumador, 
                                Snorer = Roncador)

# EXPLORATORY DATA ANALYSIS
# See what kind of data we have in the df
vis_dat(df_tmp1) 
str(df_tmp1)

color <- brewer.pal(7, "Set3") 
#length nos da el value contando los NA, en realidad es -1 de eso 

length(unique(df_tmp1$Gender)) # 2 diff. values
barplot(table(df_tmp1$Gender), ylim = c(0,550), main= "Genders", col=color)

length(unique(df_tmp1$Smoker)) # 6 diff. values
barplot(table(df_tmp1$Smoker), ylim = c(0,550),main= "Smokers", col=color)

length(unique(df_tmp1$Snorer)) # 8 diff. values
barplot(table(df_tmp1$Snorer), ylim = c(0,550),main= "Snorer", col=color)

length(unique(df_tmp1$Patient)) # 684 diff. values
length(unique(df_tmp1$Weight)) # 92 diff. values



# FEATURES TRANSFORMATIONS

# Drop Patient feature
df_tmp1 <- subset(df_tmp1, select = -Patient)

# Factor and numeric conversion 
df_tmp1$Gender = factor(df_tmp1$Gender)
df_tmp1$Gender = as.numeric(df_tmp1$Gender)

df_tmp1$Smoker = factor(df_tmp1$Smoker)
df_tmp1$Smoker = as.numeric(df_tmp1$Smoker)

#plot(table(df_tmp1$Smoker))
df_tmp1$Snorer = factor(df_tmp1$Snorer)
df_tmp1$Snorer= as.numeric(df_tmp1$Snorer)

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

