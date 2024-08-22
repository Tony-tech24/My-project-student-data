library(readxl)
library(tidyverse)
library(dplyr)
data_student_perfomance <- read_excel("data student perfomance.xlsx")
View(data_student_perfomance)
glimpse(data_student_perfomance)
colnames(data_student_perfomance)
gender<- data_student_perfomance%>%
  select(Gender)%>%
  group_by(Gender)%>%
  summarize(count=n())%>%
  mutate( Percentage = round(count/sum(count) *100))
ggplot(gender, aes(x=Gender, y=count, fill = Gender))+
  geom_col(position = "dodge", width = 0.7)+
  scale_fill_manual(values = c("red", "orange"))+
  labs(x="Gender", y= "Count", title = "GENDER ANALYSIS")

  
  
