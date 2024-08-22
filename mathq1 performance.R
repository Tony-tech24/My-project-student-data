library(readxl)
library(tidyverse)
library(dplyr)
data_student_perfomance <- read_excel("data student perfomance.xlsx")
View(data_student_perfomance)
glimpse(data_student_perfomance)
summarise(data_student_perfomance)
is.na(data_student_perfomance)
complete.cases(data_student_perfomance)
head(data_student_perfomance)
colnames(data_student_perfomance)
mean(data_student_perfomance$MathQ1, na.rm = TRUE)
gender<- data_student_perfomance%>%
  select(Gender,MathQ1)%>%
  group_by(Gender,MathQ1)%>%
  summarize(count=n())
ggplot(gender, aes(x=MathQ1, y=count, fill = Gender))+
  geom_col(position = "dodge", width = 0.4)+
  scale_fill_manual(values = c("blue","green"))+
  labs(x="performance", y="Count", title = "PERFORMANCE OF MATHQ1")
  
  
  

