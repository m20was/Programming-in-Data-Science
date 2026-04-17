
install.packages("cowplot")
install.packages("plotrix")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("grpby")


library(cowplot)
library(readxl)
library(ggplot2)
library(dplyr)


student_performace = read_excel("C:/Users/neeli/OneDrive/Documents/PDS/Inputs/StudentsPerformance.xlsx")

View(student_performace)

head(student_performace)

tail(student_performace)

summary(student_performace)

nrow(student_performace)

str(student_performace)

print(student_performace)

grp_count = student_performace %>% count(group)
grp_count

student_performace %>% filter(is.na(gender))

student_performace %>% filter(is.na(group))

student_performace %>% filter(gender=='male' & group == 'group B')

student_performace %>% filter(group == 'group B' & math_score == 40)

new_student_performace = mutate(student_performace,average_score = round((math_score+reading_score+writing_score)/3))
new_student_performace$average_score
head(new_student_performace)

View(new_student_performace)

##Solution 1##

agg_reading_score = aggregate(new_student_performace$reading_score, list(student_performace$group), FUN=mean)
colnames(agg_reading_score) <- c('group','avg_reading_score')

agg_math_score = aggregate(new_student_performace$math_score, list(student_performace$group), FUN=mean)
colnames(agg_math_score) <- c('group','avg_math_score')

agg_average_score = aggregate(new_student_performace$average_score, list(student_performace$group), FUN=mean)
colnames(agg_average_score) <- c('group','avg_student_performance')

agg_writing_score = aggregate(new_student_performace$writing_score, list(student_performace$group), FUN=mean)
colnames(agg_writing_score) <- c('group','avg_writing_score')

p1 = ggplot(agg_math_score, aes(x=group, y=avg_math_score, fill=group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Average_Math_Score")

p2 = ggplot(agg_reading_score, aes(x=group, y=avg_reading_score,fill=group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Average_Reading_Score")

p3 = ggplot(agg_writing_score, aes(x=group, y=avg_writing_score,fill=group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Average_Writing_Score")

p4 = ggplot(agg_average_score, aes(x=group, y=avg_student_performance,fill=group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Average_Student_Performance")

plot_grid(p1,p2,p3,p4,labels="AUTO")

##soulution 2##

grp2_avg = student_performace %>% group_by(group,gender) %>% summarise(avg_math = mean(math_score))  
grp2_avg

ggplot(grp2_avg,(aes(x=group,y=avg_math,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")


##solution 3##

total_student_performace = mutate(student_performace,total_score = (math_score+ reading_score+ writing_score))
total_student_performace$total_score
head(total_student_performace)

grp3_total = total_student_performace %>% group_by(group,gender) %>% summarise(sum_total = sum(total_score))

ggplot(grp3_total,(aes(x=group,y=sum_total,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")

total_box=ggplot(total_student_performace, mapping=aes(x=group, y=total_score))+geom_boxplot()

total_box=ggplot(total_student_performace, mapping=aes(x=group, y=total_score, fill=gender))+geom_boxplot()
