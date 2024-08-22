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
School<- data_student_perfomance%>%
  select(School, LitQ1)%>%
  group_by(School, LitQ1)%>%
  summarize(count=n())
ggplot(School, aes(x=LitQ1, y=count, fill = School))+
  geom_col(position = "dodge", width = 0.4)+
  labs(x="Performance", y="Count", title = "SCHOOL PERFORMANCE OF LitQ1")