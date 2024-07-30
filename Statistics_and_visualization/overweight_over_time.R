install.packages("extrafont")
library(tidyverse)
library(ggplot2)
library(extrafont)
font_import()
loadfonts(device="win") 

#import data file
overweight <- read_csv("overweight_over_time.csv")

#create line graph
overweight

ggplot(data=overweight, aes(x=Year, y=Value, group=1)) +
  geom_smooth(method="auto", se=FALSE, colour="black")+
scale_x_continuous(name="Year", limits=c(1993, 2019), breaks=seq(1993,2019,3)) +
  scale_y_continuous(name="UK Population with Overweight or Obesity (%)", limits=c(52, 65), breaks=seq(52,65,2))+
  theme(
    axis.text = element_text(size = 14, family = "Calibri"), 
    axis.title = element_text(size = 16, family = "Calibri")
  )
  
  

