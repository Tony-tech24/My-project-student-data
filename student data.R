library(readxl)
data_student_perfomance_1_ <- read_excel("C:\\Users\\edutabuser\\Downloads\\data student perfomance (1).xlsx")
View(data_student_perfomance_1_)
head(data_student_perfomance_1_)
duplicated_rows <- data_student_perfomance_1_[duplicated(data_student_perfomance_1_), ]
duplicated_rows
missing_values <- colSums(is.na(data_student_perfomance_1_))
missing_values
is.na(data_student_perfomance_1_)
complete.cases(data_student_perfomance_1_)
summary(data_student_perfomance_1_)