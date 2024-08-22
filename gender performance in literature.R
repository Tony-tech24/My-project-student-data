#loading libraries

library(readxl)
library(tidyverse)
library(dplyr)
#viewing the data
data_student_perfomance <- read_excel("data student perfomance.xlsx")
View(data_student_perfomance)
glimpse(data_student_perfomance)
summarise(data_student_perfomance)
is.na(data_student_perfomance)
complete.cases(data_student_perfomance)
head(data_student_perfomance)
colnames(data_student_perfomance)
mean(data_student_perfomance$MathQ1, na.rm = TRUE)
Gender<- data_student_perfomance%>%
  select(Gender, LitQ1,LitQ2,LitQ3,LitQ4,LitQ5)%>%
  group_by(Gender,LitQ1,LitQ2,LitQ3,LitQ4,LitQ5)%>%
  summarize(count=n())
ggplot(Gender, aes(x=LitQ1,LitQ2,LitQ3,LitQ4,LitQ5, y= count, fill = School) )+
  geom_bar(position = "dodge", width = 0.2)+
  labs(x="Performance in literature", y="Count", title = "GENDER PERFORMANCE IN LITERATURE")