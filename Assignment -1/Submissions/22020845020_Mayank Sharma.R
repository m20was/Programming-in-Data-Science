
install.packages("readxl")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("cowplot")
install.packages("tidyverse")





library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)

student = read_excel("D:/SIBMB/Students.xlsx")

View(student)

head(student)

tail(student)

summary(student)

nrow(student)

str(student)

print(student)

grp_count = student %>% count(group)
grp_count

student %>% filter(is.na(group))

student %>% filter(is.na(gender))

student %>% filter(gender=='male' & group == 'group B')

student %>% filter(group == 'group B' & math_score == 40)

new_student = mutate(student,average_score = round((math_score+reading_score+writing_score)/3))
new_student$average_score
head(new_student)

View(new_student)

#ANS 1

agg_math_score = aggregate(new_student$math_score, list(student$group), FUN=mean)
colnames(agg_math_score) <- c('group','avg_math_score')
agg_math_score

agg_reading_score = aggregate(new_student$reading_score, list(student$group), FUN=mean)
colnames(agg_reading_score) <- c('group','avg_reading_score')
agg_reading_score

agg_writing_score = aggregate(new_student$writing_score, list(student$group), FUN=mean)
colnames(agg_writing_score) <- c('group','avg_writing_score')
agg_writing_score

agg_average_score = aggregate(new_student$average_score, list(student$group), FUN=mean)
colnames(agg_average_score) <- c('group','avg_student_performance')
agg_average_score

dev.off(dev.list()["RStudioGD"]) 

p1 = ggplot(agg_math_score, aes(x=group, y=avg_math_score, fill=group)) + geom_bar(stat="identity") + 
        labs(x="Groups", y="Average_Math_Score")
p1

p2 = ggplot(agg_reading_score, aes(x=group, y=avg_reading_score,fill=group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Average_Reading_Score")
p2

p3 = ggplot(agg_writing_score, aes(x=group, y=avg_writing_score,fill=group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Average_Writing_Score")
p3

p4 = ggplot(agg_average_score, aes(x=group, y=avg_student_performance,fill=group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Average_Student_Performance")
p4

plot_grid(p1,p2,p3,p4,labels="AUTO")

#ANS 2

grp2_avg = student %>% group_by(group,gender) %>%
  summarise(avg_math = mean(math_score))  
grp2_avg

ggplot(grp2_avg,(aes(x=group,y=avg_math,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")


#ANS 3

total_student = mutate(student,total_score = (math_score+reading_score+writing_score))
total_student$total_score
head(total_student)

grp3_total = total_student %>% group_by(group,gender) %>%
  summarise(sum_total = sum(total_score))  
grp3_total

ggplot(grp3_total,(aes(x=group,y=sum_total,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")

total_box=ggplot(total_student, mapping=aes(x=group, y=total_score))+geom_boxplot()
total_box

total_box=ggplot(total_student, mapping=aes(x=group, y=total_score, fill=gender))+geom_boxplot()
total_box

