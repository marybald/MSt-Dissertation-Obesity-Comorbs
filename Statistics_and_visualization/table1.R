library(tidyverse)
library(ggplot2)
library(extrafont)
library(table1)


font_import()
loadfonts(device="win") 


data <- read_csv("table1_data.csv")

head(data)

#data manipulation

#display OSA status as Y/N
data$OSA_FLAG <- ifelse(data$OSA_FLAG == 1, "Yes", "No")



data %>%
  arrange(factor(BMI_CAT, levels = c("Underweight", "Healthy", "Overweight", "Class 1", "Class 2", "Class 3"), ordered = TRUE))


data$BMI_CAT <- factor(data$BMI_CAT,
                       levels = c("Underweight", "Healthy", "Overweight", "Class 1", "Class 2", "Class 3"),
                       labels = c("Underweight", "Healthy", "Overweight", "Class 1", "Class 2", "Class 3"))


label(data$AGE_GRP)      <- "Age Group"
label(data$BMI_CAT)      <- "BMI Category, adjusted for ethnicity"
label(data$BMI)      <- "BMI (continuous)"
label(data$GENDER)    <- "Gender"
label(data$OSA_FLAG)    <- "OSAHS Status"
label(data$HT_STAGE)  <- "Hypertension Category"
label(data$IHD_CAT)   <- "Ischaemic Heart Disease Category"



#creating and presenting the table

table1(~AGE_GRP + BMI_CAT + BMI + GENDER + OSA_FLAG + HT_STAGE | IHD_CAT, data=data, render.continuous="Mean (SD)", topclass="Rtable1-shade Rtable1-calibri")


