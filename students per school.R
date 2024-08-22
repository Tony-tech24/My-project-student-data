#loading libraries
> library(readxl)
#importing libraries
> data_student_perfomance <- read_excel("C:/Users/edutabuser/Downloads/data student perfomance.xlsx", 
                                        +     sheet = "Level_2_Assessment")
> View(data_student_perfomance)
glimpse(data_student_perfomance)
colnames(data_student_perfomance)
School<- data_student_perfomance %>%
  select(School)%>%
  group_by(School)%>%
  summarize(count=n())
ggplot(School, aes(y=count, x=School,fill=School))+ 
  geom_col(position= "dodge", width = 0.5)+
  scale_fill_manual(values = c("red", "green"))+
  labs (x ="schools", y ="number of students", 
        title = "Number of students per school")
