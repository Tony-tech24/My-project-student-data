#loading libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
endlinedata <- read_excel("C:/Users/edutabuser/Downloads/endlinedata.xlsx", 
                          sheet = "Level_1_endline")
View(endlinedata)
#cleaning the data 
head(endlinedata)
is.na(endlinedata)
glimpse(endlinedata)
duplicates<- endlinedata[duplicated(endlinedata),]
#removing duplicates
unique.data.frame <- distinct(endlinedata)
print(unique.data.frame)
#replacing NA with 0
unique.data.frame[is.na(unique.data.frame)] <- 0.0
colnames(unique.data.frame)
#getting total scores for math
unique.data.frame<- unique.data.frame%>%
  mutate(Total_math=rowSums(select(.,starts_with("MathQ"))))
#gender performance in math
Gender<- unique.data.frame%>%
  select(Gender, Total_math)%>%
  group_by(Gender, Total_math)%>%
  summarize(count=n())
#plotting the graph
ggplot(Gender, aes(x=Total_math, y=count, fill = Gender))+
  geom_col(position = "dodge", width = 0.5)+
  geom_text(aes(label = count),position=position_dodge(width=0.5),vjust = 0.01)+
  scale_x_continuous(limits = c(2.0, 15.0), breaks = seq(2.0, 15.0, by = 0.5))+
  scale_fill_manual(values = c("blue","green"))+
  labs(x="Performance in math", y="Number od students", title = "GENDER PERFORMANCE IN MATH.")

#gender performance in literature
#getting total scores in Lit
unique.data.frame<- unique.data.frame%>%
  mutate(Total_Lit=rowSums(select(.,starts_with("LitQ"))))
Gender<- unique.data.frame%>%
  select(Gender, Total_Lit)%>%
  group_by(Gender, Total_Lit)%>%
  summarize(count=n())
#plotting the graph
ggplot(Gender, aes(x=Total_Lit, y=count, fill = Gender))+
  geom_col(position = "dodge", width = 0.5)+
  geom_text(aes(label = count),position=position_dodge(width=0.5),vjust = 0.01)+
  scale_x_continuous(limits = c(0, 8), breaks = seq(0, 8, by = 0.5))+
  scale_fill_manual(values = c("grey","violet"))+
  labs(x="Performance in Literature", y="Number od students", title = "GENDER PERFORMANCE IN LITERATURE.")

#gender performance in science
#getting total science scores
unique.data.frame<- unique.data.frame%>%
  mutate(Total_Sci=rowSums(select(.,starts_with("SciQ"))))
Gender<- unique.data.frame%>%
  select(Gender, Total_Sci)%>%
  group_by(Gender, Total_Sci)%>%
  summarize(count=n())
#plotting the graph
ggplot(Gender, aes(x=Total_Sci, y=count, fill = Gender))+
  geom_col(position = "dodge", width = 0.5)+
  geom_text(aes(label = count),position=position_dodge(width=0.5),vjust = 0.01)+
  scale_x_continuous(limits = c(0, 14), breaks = seq(0, 14, by = 0.5))+
  scale_fill_manual(values = c("orange","maroon"))+
  labs(x="Performance in science", y="Number od students", title = "GENDER PERFORMANCE IN SCIENCE.")

#number of students per school
Gender<- unique.data.frame%>%
  select(Gender, School)%>%
  group_by(Gender, School)%>%
  summarize(count=n())
#plotting the graph
ggplot(Gender, aes(x=School, y=count, fill = Gender))+
  geom_col(position = "dodge", width = 1)+
  geom_text(aes(label = count),position=position_dodge(width=0.5),vjust = 0.01)+
  scale_fill_manual(values = c("brown","purple"))+
  labs(x="Schools", y="Number od students", title = "NUMBER OF STUDENTS PER GENDER IN SCHOOLS.")

#level 1 c skills
#loaging libraries
library(tidyverse)
library(dbplyr)
library(ggplot2)
library(readxl)
endlinedata <- read_excel("C:/Users/edutabuser/Downloads/endlinedata.xlsx", 
                          sheet = "Level_1_C_Skills")
View(endlinedata)
#cleaning the data 
head(endlinedata)
is.na(endlinedata)
glimpse(endlinedata)
#removing duplicates
duplicates<- endlinedata[duplicated(endlinedata),]
unique.df <- distinct(endlinedata)
print(unique.df)
#replacing NA with 0
unique.df[is.na(unique.df)] <- 0.0
colnames(unique.df)
#removing the date column
unique.df<- select(unique.df, - Date)
print(unique.df)
#gender performance in C skills
#getting total C skills scores
unique.df<- unique.df%>%
  mutate(Total_C=rowSums(select(.,starts_with("C"))))
Gender<- unique.df%>%
  select(Gender, Total_C)%>%
  group_by(Gender, Total_C)%>%
  summarize(count=n())

#plotting the graph
ggplot(Gender, aes(x=Total_C, y=count, fill = Gender))+
  geom_col(position = "dodge", width = 0.5)+
  geom_text(aes(label = count),position=position_dodge(width=0.5),vjust = 0.01)+
  scale_x_continuous(limits = c(1, 12), breaks = seq(1, 12, by = 1))+
  scale_fill_manual(values = c("violet","pink"))+
  labs(x="Performance in C skills", y="Number od students", title = "GENDER PERFORMANCE IN C SKILLS.")

#school performance in C skills
School<- unique.df%>%
  select(School, Total_C)%>%
  group_by(School, Total_C)%>%
  summarize(count=n())
#plotting the graph
ggplot(School, aes(x=Total_C, y=count, fill = School))+
  geom_col(position = "dodge", width = 0.5)+
  geom_text(aes(label = count),position=position_dodge(width=0.5),vjust = 0.01)+
  scale_x_continuous(limits = c(1, 12), breaks = seq(1, 12, by = 1))+
  scale_fill_manual(values = c("brown","grey"))+
  labs(x="Performance in C skills", y="Number od students", title = "SCHOOL PERFORMANCE IN C SKILLS.")

#level 2 endline dataset
#loaging libraries
library(tidyverse)
library(tidyr)
library(dplyr)
library(readxl)
endlinedata <- read_excel("C:/Users/edutabuser/Downloads/endlinedata.xlsx", 
                          sheet = "Level_2_Endline_new")
View(endlinedata)
#removing duplicates
duplicates<- endlinedata[duplicated(endlinedata),]
unique.df <- distinct(endlinedata)
print(unique.df)
#replacing NA with 0
unique.df[is.na(unique.df)] <- 0
colnames(unique.df)
unique.df<- unique.df%>%
  mutate(Total_Math=rowSums(select(.,starts_with("MathQ"))))

#gender performance in math
Gender<- unique.df%>%
  select(Gender, Total_Math)%>%
  group_by(Gender, Total_Math)%>%
  summarize(count=n())
#plotting the graph
ggplot(Gender, aes(x=Total_Math, y=count, fill = Gender))+
  geom_col(position = "dodge", width = 0.5)+
  geom_text(aes(label = count),position=position_dodge(width=0.5),vjust = 0.01)+
  scale_x_continuous(limits = c(1, 18), breaks = seq(1, 18, by = 0.5))+
  scale_fill_manual(values = c("blue","green"))+
  labs(x="Performance in math", y="Number od students", title = "GENDER PERFORMANCE IN MATH LEVEL 2.")

#gender performance in literature
#getting total scores in Lit
unique.df<- unique.df%>%
  mutate(Total_Lit=rowSums(select(.,starts_with("LitQ"))))
Gender<- unique.df%>%
  select(Gender, Total_Lit)%>%
  group_by(Gender, Total_Lit)%>%
  summarize(count=n())
#plotting the graph
ggplot(Gender, aes(x=Total_Lit, y=count, fill = Gender))+
  geom_col(position = "dodge", width = 0.5)+
  geom_text(aes(label = count),position=position_dodge(width=0.5),vjust = 0.01)+
  scale_x_continuous(limits = c(0, 8), breaks = seq(0, 8, by = 0.5))+
  scale_fill_manual(values = c("grey","violet"))+
  labs(x="Performance in Literature", y="Number od students", title = "GENDER PERFORMANCE IN LITERATURE LEVEL 2.")

#gender performance in science
#getting total science scores
unique.df<- unique.df%>%
  mutate(Total_Sci=rowSums(select(.,starts_with("SciQ"))))
Gender<- unique.df%>%
  select(Gender, Total_Sci)%>%
  group_by(Gender, Total_Sci)%>%
  summarize(count=n())
#plotting the graph
ggplot(Gender, aes(x=Total_Sci, y=count, fill = Gender))+
  geom_col(position = "dodge", width = 0.5)+
  geom_text(aes(label = count),position=position_dodge(width=0.5),vjust = 0.01)+
  scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 0.5))+
  scale_fill_manual(values = c("orange","maroon"))+
  labs(x="Performance in science", y="Number od students", title = "GENDER PERFORMANCE IN SCIENCE LEVEL 2.")

#how schools performed in science
School<- unique.df%>%
  select(School, Total_Sci)%>%
  group_by(School, Total_Sci)%>%
  summarize( count=n())
#plotting the graph
ggplot(School, aes(x=Total_Sci, y=count, fill = School))+
  geom_col(position = "dodge", width = 0.5)+
  geom_text(aes(label = count),position=position_dodge(width=0.5),vjust = 0.01)+
  scale_x_continuous(limits = c(0.0, 10), breaks = seq(0.0, 10, by = 0.5))+
  scale_fill_manual(values = c("yellow","green"))+
  labs(x="Performance in science", y="Number od students", title = "SCHOOL PERFORMANCE IN SCIENCE LEVEL 2.")

#level 2 C skills
#gender performance i n c skills
#loaging libraries
library(tidyverse)
library(dbplyr)
library(ggplot2)
library(readxl)
endlinedata <- read_excel("C:/Users/edutabuser/Downloads/endlinedata.xlsx", 
                          sheet = "Level_2_C_skills_new")
View(endlinedata)
duplicates<- endlinedata[duplicated(endlinedata),]
unique.df <- distinct(endlinedata)
print(unique.df)
#replacing NA with 0
unique.df[is.na(unique.df)] <- 0.0
colnames(unique.df)
#removing the date column
unique.df<- select(unique.df, - Date)
print(unique.df)
#gender performance in C skills
#getting total C skills scores
unique.df<- unique.df%>%
  mutate(Total_C=rowSums(select(.,starts_with("C"))))
Gender<- unique.df%>%
  select(Gender, Total_C)%>%
  group_by(Gender, Total_C)%>%
  summarize(count=n())
#plotting the graph
ggplot(Gender, aes(x=Total_C, y=count, fill = Gender))+
  geom_col(position = "dodge", width = 0.5)+
  geom_text(aes(label = count),position=position_dodge(width=0.5),vjust = 0.01)+
  scale_x_continuous(limits = c(1, 12), breaks = seq(1, 12, by = 1))+
  scale_fill_manual(values = c("violet","pink"))+
  labs(x="Performance in C skills", y="Number od students", title = "GENDER PERFORMANCE IN C SKILLS LEVEL 2.")

#School performance in C skills
School<- unique.df%>%
  select(School, Total_C)%>%
  group_by(School, Total_C)%>%
  summarize(count=n())
#plotting the graph
ggplot(School, aes(x=Total_C, y=count, fill = School))+
  geom_col(position = "dodge", width = 0.5)+
  geom_text(aes(label = count),position=position_dodge(width=0.5),vjust = 0.01)+
  scale_x_continuous(limits = c(1, 12), breaks = seq(1, 12, by = 1))+
  scale_fill_manual(values = c("brown","grey"))+
  labs(x="Performance in C skills", y="Number od students", title = "SCHOOL PERFORMANCE IN C SKILLS")




