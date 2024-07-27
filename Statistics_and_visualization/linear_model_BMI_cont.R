library(ggResidpanel)
library (rstatix)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(grid)
library(gt)
library(broom)
library(forestmangr)
library(forestplot)
library(ggforestplot)
library(extrafont)


font_import()
yloadfonts(device="win") 


data <- read_csv("table1_data.csv")
#display OSA status as Y/N
data$OSA_FLAG <- ifelse(data$OSA_FLAG == 1, "Yes", "No")

print (data, n=100)


# THIS SECTION IS ABOUT BMI (CONTINUOUS) AGAINST THE OTHER VARIABLES


#BMI and IHD Category
##visualise data
BMI_IHD <- data %>%
  ggplot(aes(x = IHD_CAT, y = BMI)) +
  geom_boxplot() +
  labs(x = "IHD Category", y = "BMI (continuous)") +
  ggtitle("BMI by IHD endpoint category")+
  theme(text = element_text(family = "Times New Roman"))
BMI_IHD

##ANOVA BMI continuous against IHD category
ANOVA_IHD <- aov(BMI ~ IHD_CAT, data=data)
summary(ANOVA_IHD)

##Tukey's test
tukey_result <- TukeyHSD(ANOVA_IHD, conf.level = 0.95)
tukey_result
##The means are significantly different between angina pectoris and MI and between CAD and MI, but not between CAD and angina pectoris



#BMI and OSAHS Status
##visualise data
BMI_OSAHS <- data %>% 
  ggplot(aes(x = OSA_FLAG, y = BMI)) +
  geom_boxplot() +
  labs(x = "OSAHS Status", y = "BMI (continuous)")+
  ggtitle("BMI by OSAHS status")+
  theme(text = element_text(family = "Times New Roman"))
BMI_OSAHS

##ANOVA
ANOVA_OSAHS <- aov(BMI ~ OSA_FLAG, data=data)
summary(ANOVA_OSAHS)
#The the null hypothesis can be rejected and the means are significantly different. 


#BMI and HT Stage
##visualise data
BMI_HT <-
data %>% 
  ggplot(aes(x = HT_STAGE, y = BMI)) +
  geom_boxplot()+
  labs(x = "Hypertension Stage", y = "BMI (continuous)")+
  ggtitle("BMI by hypertension stage")+
  theme(text = element_text(family = "Times New Roman"))
BMI_HT

##ANOVA 
ANOVA_HT <- aov(BMI ~ HT_STAGE, data=data)
summary(ANOVA_HT)

##Tukey's test
tukey_result <- TukeyHSD(ANOVA_HT, conf.level = 0.95)
tukey_result
#None of the pairs of mean comparisons showed significant difference. Stage 3 hypertension was much higher, but was only present in a single individual

#BMI and AGE
##visualise data
BMI_AGE <-
data %>% 
  ggplot(aes(x = AGE_GRP, y = BMI)) +
  geom_boxplot()+
  labs(x = "Age Group", y = "BMI (continuous)")+
  ggtitle("BMI by age group")+
  theme(text = element_text(family = "Times New Roman"))
BMI_AGE

##ANOVA 
ANOVA_AGE <- aov(BMI ~ AGE_GRP, data=data)
summary(ANOVA_AGE)

##Tukey's test
tukey_result <- TukeyHSD(ANOVA_AGE, conf.level = 0.95)
tukey_result
##Age group is signficant between the Over 80s and all groups except 30-39 and 50-59. 


#BMI and Gender
##visualise data
BMI_Gender <- data %>% 
  ggplot(aes(x = GENDER, y = BMI)) +
  geom_boxplot() +
  labs(x = "Gender", y = "BMI (continuous)")+
  ggtitle("BMI by gender")+
  theme(text = element_text(family = "Times New Roman"))
BMI_Gender

##ANOVA
ANOVA_Gender <- aov(BMI ~ GENDER, data=data)
summary(ANOVA_Gender)
#The the null hypothesis can be rejected and the means are significantly different. 

grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 2)))
print(BMI_IHD, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(BMI_OSAHS, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(BMI_HT, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(BMI_AGE, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
print(BMI_Gender, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))



## THIS SECTION DEVELOPS A LINEAR MODEL OF MULTIPLE VARIABLES PREDICTING BMI (CONTINOUS)

lm_full <- lm(BMI~IHD_CAT * OSA_FLAG * HT_STAGE * AGE_GRP * GENDER, data = data)
summary(lm_full)

#Strongly significant relationships: MI 
#Moderately significant relationships: OSA Flag & BMI, Stage 3 hypertension & BMI, OSAHS&60-69
#Weakly significant relationships: Angina, OSA&40-49, OSA&50-59, CAD&70-79, CAD&50-59&Male, CAD&70-79&Male 


lm_backward_model <- step(lm_full, direction = 'backward', trace = 0)
summary(lm_backward_model)


#Reduced model: glm(formula = BMI ~ IHD_CAT + OSA_FLAG + HT_STAGE + AGE_GRP + GENDER + OSA_FLAG:HT_STAGE + OSA_FLAG:AGE_GRP, data = data)
#Does not reduce the AIC value much (2861 with full model, 2809 with reduced) 
#the interactions are only borderline statistically significant


#check for normality (BMI)
resid_panel(lm_full,
            plots = c("resid", "qq", "ls", "cookd"),
            smoother = TRUE)
#Result: The resid and Q-Q plots don't look too bad. Location-Scale is not too far off, and Cook's D has a number of outliers.

resid_panel(lm_backward_model,
            plots = c("resid", "qq", "ls", "cookd"),
            smoother = TRUE)
#Result: The resid and Q-Q plots don't look too bad. Location-Stale and Cook's D show signs of non-normality


# Create a forest plot of effect sizes from the reduced model

model_output <- tidy(lm_backward_model)
out_conf <- tidy(lm_backward_model, conf.int = TRUE)
lm_model_out <- round_df(out_conf, digits=2)
#lm_model_out <- lm_model_out[-1,] #remove the intercept

lm_model_out$term[lm_model_out$term == '(Intercept)'] <- 'Acute Myocardial Infarction'
lm_model_out$term[lm_model_out$term == 'IHD_CATAngina Pectoris'] <- 'Angina Pectoris'
lm_model_out$term[lm_model_out$term == 'IHD_CATCoronary Artery Disease'] <- 'Coronary Artery Disease'
lm_model_out$term[lm_model_out$term == 'OSA_FLAGYes'] <- 'OSAHS = Yes'
lm_model_out$term[lm_model_out$term == 'HT_STAGEStage 1'] <- 'Hypertension Stage 1'
lm_model_out$term[lm_model_out$term == 'HT_STAGEStage 2'] <- 'Hypertension Stage 2'
lm_model_out$term[lm_model_out$term == 'HT_STAGEStage 3'] <- 'Hypertension Stage 3'
lm_model_out$term[lm_model_out$term == 'AGE_GRP40-49'] <- 'Age 40-49'
lm_model_out$term[lm_model_out$term == 'AGE_GRP50-59'] <- 'Age 50-59'
lm_model_out$term[lm_model_out$term == 'AGE_GRP60-69'] <- 'Age 60-69'
lm_model_out$term[lm_model_out$term == 'AGE_GRP70-79'] <- 'Age 70-79'
lm_model_out$term[lm_model_out$term == 'AGE_GRPOver 80'] <- 'Age Over 80'
lm_model_out$term[lm_model_out$term == 'GENDERMale'] <- 'Male'
lm_model_out$term[lm_model_out$term == 'OSA_FLAGYes:HT_STAGEStage 1'] <- 'OSAHS:Hypertension Stage 1'
lm_model_out$term[lm_model_out$term == 'OSA_FLAGYes:HT_STAGEStage 2'] <- 'OSAHS:Hypertension Stage 2'
lm_model_out$term[lm_model_out$term == 'OSA_FLAGYes:HT_STAGEStage 3'] <- 'OSAHS:Hypertension Stage 3'
lm_model_out$term[lm_model_out$term == 'OSA_FLAGYes:AGE_GRP40-49'] <- 'OSAHS: Age 40-49'
lm_model_out$term[lm_model_out$term == 'OSA_FLAGYes:AGE_GRP50-59'] <- 'OSAHS: Age 50-59'
lm_model_out$term[lm_model_out$term == 'OSA_FLAGYes:AGE_GRP60-69'] <- 'OSAHS: Age 60-69'
lm_model_out$term[lm_model_out$term == 'OSA_FLAGYes:AGE_GRP70-79'] <- 'OSAHS: Age 70-79'
lm_model_out$term[lm_model_out$term == 'OSA_FLAGYes:AGE_GRPOver 80'] <- 'OSAHS: Age Over 80'


lm_model_out

forest_plot <- ggplot(lm_model_out, aes(x = estimate, xmin = conf.low, xmax = conf.high, y = reorder(term,estimate))) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(height = 0.2, color = "blue") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +
  labs(x = "Effect size", y = "Predictor") +
  theme_minimal()+
  theme(text = element_text(family = "Times New Roman")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
forest_plot

