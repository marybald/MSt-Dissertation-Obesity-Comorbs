library(tidyverse)
library(ggplot2)

#import data file

obesity <- read_csv("obesity_over_time.csv")

obesity

ggplot(data=obesity, aes(x=Year, y=Value, group=1)) +
  geom_smooth(method="auto", se=FALSE, colour="black")+
  scale_x_discrete (name="Year") +
  scale_y_continuous(name="UK Population with Obesity (%)", limits=c(22, 27), breaks=seq(22,27,1))+
  theme(
    axis.text = element_text(size = 14, family = "Calibri"), 
    axis.title = element_text(size = 16, family = "Calibri")
  )
