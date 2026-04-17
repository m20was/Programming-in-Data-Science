install.packages("readxl")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("cowplot")
install.packages("tidyverse")

library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)

score = read_excel("C:\Users\Raiso\OneDrive\Documents\pds xlsx file")

View(score)

head(score)

tail(score)

summary(score)

nrow(score)

str(score)

print(score)


score %>% filter(is.na(group))

score %>% filter(is.na(gender))

score %>% filter(gender=='male' & group == 'group C')

score %>% filter(group == 'group B' & reading_score == 40)

new_score = mutate(score,average_score = round((math_score+reading_score+writing_score)/3))
new_score$average_score
head(new_score)

View(new_score)

#Analyze the performance of one of the groups ***
#ANSWER NO 1

aggregate_math_score = aggregate(new_score$math_score, list(score$group), FUN=mean)
colnames(aggregate_math_score) <- c('group','avg_math_score')
aggregate_math_score

aggregate_reading_score = aggregate(new_score$reading_score, list(score$group), FUN=mean)
colnames(aggregate_reading_score) <- c('group','avg_reading_score')
aggregate_reading_score

aggregate_writing_score = aggregate(new_score$writing_score, list(score$group), FUN=mean)
colnames(aggregate_writing_score) <- c('group','avg_writing_score')
aggregate_writing_score

aggregate_average_score = aggregate(new_score$average_score, list(score$group), FUN=mean)
colnames(aggregate_average_score) <- c('group','avg_student_performance')
aggregate_average_score

dev.off(dev.list()["RStudioGD"]) 

p1 = ggplot(aggregate_math_score, aes(x=group, y=avg_math_score, fill=group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Average_Maths_Score")
p1

p2 = ggplot(aggregate_reading_score, aes(x=group, y=avg_reading_score,fill=group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Average_Reading_Score")
p2

p3 = ggplot(aggregate_writing_score, aes(x=group, y=avg_writing_score,fill=group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Average_Writing_Score")
p3

p4 = ggplot(aggregate_average_score, aes(x=group, y=avg_student_performance,fill=group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Average_Student_Performance")
p4

plot_grid(p1,p2,p3,p4,labels="AUTO")

#Analyze the gender performance in Math for each group ***
#ANSWER NO 2

group2_avg = score %>% group_by(group,gender) %>%
  summarise(avg_math = mean(math_score))  
group2_avg

ggplot(group2_avg,(aes(x=group,y=avg_math,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")

#Add a new column “total mark” and analyze the group performance with respect to Total Marks ***
#ANSWER NO 3

total = mutate(score,total = (math_score+reading_score+writing_score))
total$total
head(total)
View(total)

group3_total = total %>% group_by(group,gender) %>%
  summarise(sum_total = sum(total))  
group3_total

ggplot(group3_total,(aes(x=group,y=sum_total,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")

total_box=ggplot(total, mapping=aes(x=group, y=total))+geom_boxplot()
total_box

total_box=ggplot(total, mapping=aes(x=group, y=total, fill=gender))+geom_boxplot()
total_box