library(tidyverse)

data_student_perfomance <- read_excel("C:/Users/edutabuser/Downloads/data student perfomance.xlsx", 
                                      +     sheet = "Level_2_Assessment")
View(data_student_perfomance)
glimpse(data_student_perfomance)
mean(data_student_perfomance$MathQ1, na.rm = TRUE)
head(data_student_perfomance)
tail(data_student_perfomance)
colnames(data_student_perfomance)
length(data_student_perfomance)
names(data_student_perfomance)
is.na(data_student_perfomance)
data_student_perfomance[data_student_perfomance == NA ] <- 0
na.omit(is.na(data_student_perfomance))%>%
  select(data_student_perfomance)%>%
  filter(complete.cases())
data_student_perfomance%>%
  select(MathQ1)%>%
  count(MathQ1)%>%
  arrange(desc(n))%>%
  view()

