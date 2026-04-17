
install.packages("readxl")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("cowplot")
install.packages("tidyverse")

library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)

student = read_excel("C:/Users/JASHN SINGH/OneDrive/Desktop/MBA/1st sem/Programming for data sciences/StudentsPerformance.xlsx")
view(student)
summary(student)
nrow(student)
str(student)
tail(student)
head(student)
colors = seq(612,656,by=3)


student$group
student$gender

#Total no of students(female and male)
table(student$gender)


#To find total no. of students in a group. 
total_students= table(student$group)
#in coloumn format 
cbind(total_students)

#total students in a group
total= table(student)
plot(table(student$group))

#to print no of students and their scores in coloumn format 


boom=table(student$group)
cbind(boom)

boom_math=table(student$math_score)
cbind(boom_math)

boom_reading= table(student$reading_score)
cbind(boom_reading)

boom_writing=table(student$writing_score)

cbind(boom_writing)

#total students in gender
TotalGender= table(student$gender)
plot(table(student$gender))

table(student$gender)

#Graph to know count of students and their scores using GGPLOT 

ggplot(student, aes(x=math_score)) + 
  geom_freqpoly(binwidth=1,size=1) 

ggplot(student, aes(x=reading_score)) + 
  geom_freqpoly(binwidth=1.5,size=1.5)

ggplot(student, aes(x=writing_score)) + 
  geom_freqpoly(binwidth=1,size=1)
#boxplot
p1_box=ggplot(student, mapping=aes(x=math_score, y=gender))+geom_boxplot()

p1_box


p2_box=ggplot(student, mapping=aes(x=reading_score, y=group))+geom_boxplot()
p2_box

#no of students by their group who's score is more than 40 in maths and so on.
#Geombar
x2= student %>% 
  group_by(group) %>%
  select(math_score,reading_score,writing_score) %>%
  filter(math_score>40)
x2 %>% ggplot() + geom_bar(aes(x2$group,fill = x2$group))



x1= student %>% 
  group_by(gender) %>%
  select(math_score,reading_score,writing_score) %>%
  filter(math_score > 40) 
x1 %>% ggplot() + geom_bar(aes(x1$gender,fill = x1$gender))

x3= student %>%
  group_by(math_score) %>%
  select(math_score,reading_score,writing_score) %>%
  filter(writing_score>50)
x3 %>% ggplot() + geom_bar(aes(x3$math_score,fill = x3$math_score))

#a new column for total marks 

new_marks = mutate(student,total_marks = round(math_score+reading_score+writing_score))
new_marks$total_marks
new= table(new_marks$total_marks)
cbind(new)
