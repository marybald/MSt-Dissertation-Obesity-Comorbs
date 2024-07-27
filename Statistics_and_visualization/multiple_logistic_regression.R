library(ggResidpanel)
library (rstatix)
library(tidyverse)
library(ggplot2)
library(nnet)
library(gtsummary)
library(lmtest)


data <- read_csv("table1_data.csv")
#display OSA status as Y/N
data$OSA_FLAG <- ifelse(data$OSA_FLAG == 1, "Yes", "No")

#LOGISTIC REGRESSION 

data$IHD_CAT <- factor(data$IHD_CAT)
data$New_IHD <- relevel(data$IHD_CAT, ref = "Acute Myocardial Infarction")

#generate full model with all variables. Do not include interactions at this point as the model becomes too complex (too many weights)
model <- multinom(New_IHD ~ AGE_GRP + HT_STAGE + GENDER + BMI_CAT + OSA_FLAG, data = data)

#summarise the model and generate 
summary(model)

#check if interaction variables should be added via forward stepwise regression on the full model
forward_model <- step(model, direction = 'forward', trace = 0)
summary(forward_model)
#the forward model does not add any interactions; only the individual effects are preserved

#check if the model should be simplified via backward stepwise regression on the full model
backward_model <-step(model, direction = 'backward', trace = 0)
summary(backward_model)

#the backward model reduces to: multinom(formula = New_IHD ~ GENDER + BMI_CAT, data = data)


#check performance of the model via Likelihood ratio test
lr_result <- lrtest(model, backward_model)
lr_result

#Likelihood ratio test result:
#Likelihood ratio test

#Model 1: New_IHD ~ AGE_GRP + HT_STAGE + GENDER + BMI_CAT + OSA_FLAG
#Model 2: New_IHD ~ GENDER + BMI_CAT
#Df  LogLik  Df  Chisq Pr(>Chisq)
#1  32 -571.43                      
#2  14 -582.64 -18 22.421     0.2138
# On this basis we fail to reject the null hypothesis. The models explain the data equally, thus we should choose the reduced model.

# check whether we could further reduce the model to just BMI 
BMI_only_model <- multinom(New_IHD ~ BMI_CAT, data = data)

#likelihood ratio test against the BMI + Gender model
lr_result <- lrtest(backward_model, BMI_only_model)
lr_result


# Results:
#Model 1: New_IHD ~ GENDER + BMI_CAT
#Model 2: New_IHD ~ BMI_CAT
#Df  LogLik Df  Chisq Pr(>Chisq)   
#1  14 -582.64                        
#2  12 -588.88 -2 12.487   0.001943 **
#On this basis, we reject the null hypothesis. BMI + gender is better than BMI alone

#check whether we could further reduce the model to just Gender 
Gender_only_model <- multinom(New_IHD ~ GENDER, data = data)
lr_result <- lrtest(backward_model, Gender_only_model)
lr_result
#Result:
# Model 1: New_IHD ~ GENDER + BMI_CAT
# Model 2: New_IHD ~ GENDER
#Df  LogLik  Df  Chisq Pr(>Chisq)    
#1  14 -582.64                          
#2   4 -598.20 -10 31.114  0.0005619 ***
#On this basis, we reject the null hypothesis. BMI + gender is better than gender alone. 

