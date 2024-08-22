library(readxl)
library(tidyverse)
library(dplyr)

data_student_perfomance <- read_excel("data student perfomance.xlsx")
View(data_student_perfomance)
glimpse(data_student_perfomance)
summarise(data_student_perfomance)
complete.cases(data_student_perfomance)
colnames(data_student_perfomance)

School<- data_student_perfomance %>%
  select(School)%>%
  group_by(School)%>%
  summarize(count=n())
ggplot(School, aes(y=count, x=School, fill = School))+
  geom_col(position = "dodge", width = 0.4)+
  scale_fill_manual(values = c("blue", "maroon"))+
  labs(x="Schools", y="Number of students", title = "NUMBER OF STUDENTS PER SCHOOL")




