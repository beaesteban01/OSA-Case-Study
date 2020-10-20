########################################
# OSA data analysis using:
#         - linear regression models
#

Input_file <- "OSA_DB_UPM.xlsx"
Output_file <- "OSA_extreme_male.xlsx"
Data_Directory <- "/Users/beatrizesteban/OneDrive - Universidad Politécnica de Madrid/Segundo MUIT/PRDL&MLLB/OSA/DATA/"

########################################
# OSA data analysis using:
#         - linear regression models
#


# Using readxl package to read an Excel file
# Install the readxl package is nor already installed
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


df_OSA <- read_excel(paste(Data_Directory, Input_file, sep = ""))


df_OSA = as.data.frame(df_OSA)
names(df_OSA)
dim(df_OSA)

df_OSA_male = subset(df_OSA, Gender==1)

df_OSA_male2 <- df_OSA_male %>%
            mutate(OSA = ifelse(IAH<=10, "Healthy",
            ifelse(IAH>=30, "Severe", "Mild")))
#filtrar los mild y quedarnos con severe y healthy
#hacerlos biennnnnn
df_OSA_male3 <- filter(df_OSA_male2$OSA=="Mild")

######### SAVING CLEAN DATA ##################################

write_xlsx(df_OSA_male2,
           paste(Data_Directory, Output_file, sep = ""))
