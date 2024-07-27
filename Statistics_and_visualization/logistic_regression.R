library(ggResidpanel)
library (rstatix)
library(tidyverse)
library(ggplot2)
library(nnet)
library(gtsummary)


data <- read_csv("table1_data.csv")
#display OSA status as Y/N
data$OSA_FLAG <- ifelse(data$OSA_FLAG == 1, "Yes", "No")

print (data, n=100)

#LOGISTIC REGRESSION 

#Univariate: BMI_CAT against IHD_CAT

#make MI the baseline value
data$IHD_CAT <- factor(data$IHD_CAT)
data$New_IHD <- relevel(data$IHD_CAT, ref = "Acute Myocardial Infarction")

#make Healthy weight the baseline value
data$New_BMI <- factor(data$BMI_CAT,
                                  levels = c("Healthy", "Overweight", "Class 1", "Class 2", "Class 3", "Underweight"),
                                  labels = c("Healthy", "Overweight", "Class 1", "Class 2", "Class 3", "Underweight"))


attr(data$New_BMI, "label") <- "BMI Category"

#create the model
model_BMIIHD <- multinom(New_IHD ~ New_BMI, data = data)
summary(model_BMIIHD)


model_BMIIHD %>%
  tbl_regression(exponentiate = TRUE) %>%
  as_gt() %>%
  gt::tab_options(table.font.names = "Times New Roman")


#Univariate: OSA_FLAG against IHD_CAT
#create the model

attr(data$OSA_FLAG, "label") <- "OSA Present"

#make MI the baseline value
data$IHD_CAT <- factor(data$IHD_CAT)
data$New_IHD <- relevel(data$IHD_CAT, ref = "Acute Myocardial Infarction")


model_OSAIHD <- multinom(New_IHD ~ OSA_FLAG, data = data)
summary(model_OSAIHD)

model_OSAIHD %>%
  tbl_regression(exponentiate = TRUE) %>%
  as_gt() %>%
  gt::tab_options(table.font.names = "Times New Roman")


#Univariate: GENDER against IHD_CAT
attr(data$GENDER, "label") <- "Gender"

data$New_IHD <- relevel(data$IHD_CAT, ref = "Acute Myocardial Infarction")

model_GenderIHD <- multinom(New_IHD ~ GENDER, data = data)
summary(model_GenderIHD)

model_GenderIHD %>%
  tbl_regression(exponentiate = TRUE) %>%
  as_gt() %>%
  gt::tab_options(table.font.names = "Times New Roman")

#Univariate: HT_STAGE against IHD_CAT


data$New_IHD <- relevel(data$IHD_CAT, ref = "Acute Myocardial Infarction")

data$New_HT_STAGE <- factor(data$HT_STAGE,
                        levels = c("Normal", "Stage 1", "Stage 2"),
                        labels = c("Normal", "Stage 1", "Stage 2"))

attr(data$New_HT_STAGE, "label") <- "Hypertension Stage"

model_HTIHD <- multinom(New_IHD ~ New_HT_STAGE, data = data)
summary(model_HTIHD)

model_HTIHD %>%
  tbl_regression(exponentiate = TRUE) %>%
  as_gt() %>%
  gt::tab_source_note(gt::md("*Stage 3 hypertension excluded from analysis as<br> only one subject had this reading*"))%>%
  gt::tab_options(table.font.names = "Times New Roman")



#Univariate: AGE_GRP against IHD_CAT


data$New_IHD <- relevel(data$IHD_CAT, ref = "Acute Myocardial Infarction")

data$New_AGE_GRP <- factor(data$AGE_GRP,
                           levels = c("40-49", "50-59", "60-69", "70-79", "Over 80"),
                           labels = c("40-49", "50-59", "60-69", "70-79", "Over 80"))

attr(data$New_AGE_GRP, "label") <- "Age Group"

model_AGEIHD <- multinom(New_IHD ~ New_AGE_GRP, data = data)
summary(model_AGEIHD)

model_AGEIHD %>%
  tbl_regression(exponentiate = TRUE) %>%
  as_gt() %>%
  gt::tab_source_note(gt::md("*Age group 30-39 was excluded from the analysis due to <br>low numbers within the study population*"))%>%
  gt::tab_options(table.font.names = "Times New Roman")


#________________________________________________________________________

#Create a forest plot of all the log odds effect sizes
# Create a forest plot of effect sizes IHD ~ BMI

model_out_bmi <- tidy(model_BMIIHD)
out_conf_bmi <- tidy(model_BMIIHD, conf.int = TRUE)
lm_model_out_bmi <- round_df(out_conf_bmi, digits=2)
#lm_model_out <- lm_model_out[-1,] #remove the intercept

lm_model_out_bmi <- lm_model_out_bmi %>%
  mutate(term = paste(y.level, term))

lm_model_out_bmi <- lm_model_out_bmi %>%
  select(term, estimate, std.error, statistic, p.value, conf.low, conf.high)

lm_model_out_bmi

lm_model_out_bmi$term[lm_model_out_bmi$term == 'Angina Pectoris (Intercept)'] <- 'Angina Pectoris, Healthy weight'
lm_model_out_bmi$term[lm_model_out_bmi$term == 'Angina Pectoris New_BMIOverweight'] <- 'Angina Pectoris, Overweight'
lm_model_out_bmi$term[lm_model_out_bmi$term == 'Angina Pectoris New_BMIClass 1'] <- 'Angina Pectoris, Class 1'
lm_model_out_bmi$term[lm_model_out_bmi$term == 'Angina Pectoris New_BMIClass 2'] <- 'Angina Pectoris, Class 2'
lm_model_out_bmi$term[lm_model_out_bmi$term == 'Angina Pectoris New_BMIClass 3'] <- 'Angina Pectoris, Class 3'
lm_model_out_bmi$term[lm_model_out_bmi$term == 'Angina Pectoris New_BMIUnderweight'] <- 'Angina Pectoris, Underweight'
lm_model_out_bmi$term[lm_model_out_bmi$term == 'Coronary Artery Disease (Intercept)'] <- 'CAD, Healthy weight'
lm_model_out_bmi$term[lm_model_out_bmi$term == 'Coronary Artery Disease New_BMIOverweight'] <- 'CAD, Overweight'
lm_model_out_bmi$term[lm_model_out_bmi$term == 'Coronary Artery Disease New_BMIClass 1'] <- 'CAD, Class 1'
lm_model_out_bmi$term[lm_model_out_bmi$term == 'Coronary Artery Disease New_BMIClass 2'] <- 'CAD, Class 2'
lm_model_out_bmi$term[lm_model_out_bmi$term == 'Coronary Artery Disease New_BMIClass 3'] <- 'CAD, Class 3'
lm_model_out_bmi$term[lm_model_out_bmi$term == 'Coronary Artery Disease New_BMIUnderweight'] <- 'CAD, Underweight'


lm_model_out_bmi

forest_plot_bmi <- ggplot(lm_model_out_bmi, aes(x = estimate, xmin = conf.low, xmax = conf.high, y = reorder(term,estimate))) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(height = 0.2, color = "blue") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +
  labs(x = "Effect size", y = "Predictor") +
  theme_minimal()+
  theme(text = element_text(family = "Times New Roman")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
forest_plot_bmi

#Forest plot effect sizes IHD ~ Gender


model_out_gen <- tidy(model_GenderIHD)
out_conf_gen <- tidy(model_GenderIHD, conf.int = TRUE)
lm_model_out_gen <- round_df(out_conf_gen, digits=2)

lm_model_out_gen <- lm_model_out_gen %>%
  mutate(term = paste(y.level, term))

lm_model_out_gen <- lm_model_out_gen %>%
  select(term, estimate, std.error, statistic, p.value, conf.low, conf.high)

lm_model_out_gen

lm_model_out_gen$term[lm_model_out_gen$term == 'Angina Pectoris (Intercept)'] <- 'Angina Pectoris, Female'
lm_model_out_gen$term[lm_model_out_gen$term == 'Angina Pectoris GENDERMale'] <- 'Angina Pectoris, Male'
lm_model_out_gen$term[lm_model_out_gen$term == 'Coronary Artery Disease (Intercept)'] <- 'CAD, Female'
lm_model_out_gen$term[lm_model_out_gen$term == 'Coronary Artery Disease GENDERMale'] <- 'CAD, Male'

lm_model_out_gen

forest_plot_gen <- ggplot(lm_model_out_gen, aes(x = estimate, xmin = conf.low, xmax = conf.high, y = reorder(term,estimate))) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(height = 0.2, color = "blue") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +
  labs(x = "Effect size", y = "Predictor") +
  theme_minimal()+
  theme(text = element_text(family = "Times New Roman")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
forest_plot_gen


#Forest plot effect sizes IHD ~ Hypertension Stage


model_out_ht <- tidy(model_HTIHD)
out_conf_ht <- tidy(model_HTIHD, conf.int = TRUE)
lm_model_out_ht <- round_df(out_conf_ht, digits=2)

lm_model_out_ht <- lm_model_out_ht %>%
  mutate(term = paste(y.level, term))

lm_model_out_ht <- lm_model_out_ht %>%
  select(term, estimate, std.error, statistic, p.value, conf.low, conf.high)

lm_model_out_ht

lm_model_out_ht$term[lm_model_out_ht$term == 'Angina Pectoris (Intercept)'] <- 'Angina Pectoris, Normal HT'
lm_model_out_ht$term[lm_model_out_ht$term == 'Angina Pectoris New_HT_STAGEStage 1'] <- 'Angina Pectoris, Stage 1 HT'
lm_model_out_ht$term[lm_model_out_ht$term == 'Angina Pectoris New_HT_STAGEStage 2'] <- 'Angina Pectoris, Stage 2 HT'
lm_model_out_ht$term[lm_model_out_ht$term == 'Coronary Artery Disease (Intercept)'] <- 'CAD, Normal HT'
lm_model_out_ht$term[lm_model_out_ht$term == 'Coronary Artery Disease New_HT_STAGEStage 1'] <- 'CAD, Stage 1 HT'
lm_model_out_ht$term[lm_model_out_ht$term == 'Coronary Artery Disease New_HT_STAGEStage 2'] <- 'CAD, Stage 2 HT'

lm_model_out_ht

forest_plot_ht <- ggplot(lm_model_out_ht, aes(x = estimate, xmin = conf.low, xmax = conf.high, y = reorder(term,estimate))) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(height = 0.2, color = "blue") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +
  labs(x = "Effect size", y = "Predictor") +
  theme_minimal()+
  theme(text = element_text(family = "Times New Roman")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
forest_plot_ht



#Forest plot effect sizes IHD ~ Age Group

model_out_age <- tidy(model_AGEIHD)
out_conf_age <- tidy(model_AGEIHD, conf.int = TRUE)
lm_model_out_age <- round_df(out_conf_age, digits=2)
#lm_model_out <- lm_model_out[-1,] #remove the intercept

lm_model_out_age <- lm_model_out_age %>%
  mutate(term = paste(y.level, term))

lm_model_out_age <- lm_model_out_age %>%
  select(term, estimate, std.error, statistic, p.value, conf.low, conf.high)

lm_model_out_age

lm_model_out_age$term[lm_model_out_age$term == 'Angina Pectoris (Intercept)'] <- 'Angina Pectoris, Age 40-49'
lm_model_out_age$term[lm_model_out_age$term == 'Angina Pectoris New_AGE_GRP50-59'] <- 'Angina Pectoris, Age 50-59'
lm_model_out_age$term[lm_model_out_age$term == 'Angina Pectoris New_AGE_GRP60-69'] <- 'Angina Pectoris, Age 60-69'
lm_model_out_age$term[lm_model_out_age$term == 'Angina Pectoris New_AGE_GRP70-79'] <- 'Angina Pectoris, Age 70-79'
lm_model_out_age$term[lm_model_out_age$term == 'Angina Pectoris New_AGE_GRPOver 80'] <- 'Angina Pectoris, Age Over 80'
lm_model_out_age$term[lm_model_out_age$term == 'Coronary Artery Disease (Intercept)'] <- 'CAD, Age 40-49'
lm_model_out_age$term[lm_model_out_age$term == 'Coronary Artery Disease New_AGE_GRP50-59'] <- 'CAD, Age 50-59'
lm_model_out_age$term[lm_model_out_age$term == 'Coronary Artery Disease New_AGE_GRP60-69'] <- 'CAD, Age 60-69'
lm_model_out_age$term[lm_model_out_age$term == 'Coronary Artery Disease New_AGE_GRP70-79'] <- 'CAD, Age 70-79'
lm_model_out_age$term[lm_model_out_age$term == 'Coronary Artery Disease New_AGE_GRPOver 80'] <- 'CAD, Age Over 80'


lm_model_out_age

forest_plot_age <- ggplot(lm_model_out_age, aes(x = estimate, xmin = conf.low, xmax = conf.high, y = reorder(term,estimate))) +
  geom_point(size = 3, color = "blue") +
  geom_errorbarh(height = 0.2, color = "blue") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray") +
  labs(x = "Effect size", y = "Predictor") +
  theme_minimal()+
  theme(text = element_text(family = "Times New Roman")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
forest_plot_age



