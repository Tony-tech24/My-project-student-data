#loading libraries
library(ggplot2)
library(readxl)
library(tidyverse)
library(tidyr)
#loading the data
data_student_perfomance <- read_excel("data student perfomance.xlsx")
#viewing the data
View(data_student_perfomance)
#cleaning the data
is.na(data_student_perfomance)
glimpse(data_student_perfomance)
summarize(data_student_perfomance)
head(data_student_perfomance)
tail(data_student_perfomance)
colnames(data_student_perfomance)
data_student_perfomance<- data_student_perfomance%>%
  mutate(Total_math= rowSums(select(., starts_with("MathQ"))))
Gender<- data_student_perfomance %>%
  select(Gender, Total_math)%>%
  group_by(Gender, Total_math)%>%
  summarize(count=n())
#plotting the graph
ggplot(Gender, aes(x=Total_math, y= count, fill = Gender))+
  geom_col(position = "dodge", width = 0.3)+
  scale_x_continuous(limit = c(1,14), breaks = 1:14)+
  labs(x= "Math performance", y="Number of students", title = "GENDER PERFORMANCE IN MATH.")
  
  
#gender performance in Literature
library(ggplot2)
library(readxl)
library(tidyverse)
library(tidyr)
#loading the data
data_student_perfomance <- read_excel("data student perfomance.xlsx")
#viewing the data
View(data_student_perfomance)
#cleaning the data
is.na(data_student_perfomance)
glimpse(data_student_perfomance)
summarize(data_student_perfomance)
head(data_student_perfomance)
tail(data_student_perfomance)
colnames(data_student_perfomance)
data_student_perfomance<- data_student_perfomance%>%
  mutate(Total_Lit= rowSums(select(., starts_with("LitQ"))))
Gender<- data_student_perfomance %>%
  select(Gender, Total_Lit)%>%
  group_by(Gender, Total_Lit)%>%
  summarize(count=n())
#plotting the graph
ggplot(Gender, aes(x=Total_Lit, y=count, fill= Gender))+
  geom_col(position = "dodge", width = 0.4)+
  labs(x="Literature performance", y="Number of students", title = "GENDER PERFORMANCE IN LITERATURE")
                     
  
#gender performance in Science
library(ggplot2)
library(readxl)
library(tidyverse)
library(tidyr)
#loading the data
data_student_perfomance <- read_excel("data student perfomance.xlsx")
#viewing the data
View(data_student_perfomance)
#cleaning the data
is.na(data_student_perfomance)
glimpse(data_student_perfomance)
summarize(data_student_perfomance)
head(data_student_perfomance)
tail(data_student_perfomance)
colnames(data_student_perfomance)
data_student_perfomance<- data_student_perfomance%>%
  mutate(Total_Sci= rowSums(select(., starts_with("SciQ"))))
Gender<- data_student_perfomance %>%
  select(Gender, Total_Sci)%>%
  group_by(Gender, Total_Sci)%>%
  summarize(count=n())
ggplot(Gender, aes(x=Total_Sci, y= count, fill = Gender))+
  geom_col(position = "dodge", width = 0.3)+
  scale_x_continuous(limit = c(0,11), breaks = 0:11)+
  labs(x= "Science performance", y="Number of students", title = "GENDER PERFORMANCE IN SCIENCE.")
















