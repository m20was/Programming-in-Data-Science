library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)

perf = read_excel("C:/Users/Admin/Documents/R/Projects/StudentsPerformance.xlsx")

View(perf)

head(perf)

tail(perf)

summary(perf)

nrow(perf)

str(perf)

print(perf)

group_count = perf %>% count(group)

group_count

perf %>% filter(is.na(group))

perf %>% filter(is.na(gender))

perf %>% filter(gender=='male' & group == 'group B')

perf %>% filter(group == 'group B' & math_score == 40)

new_perf = mutate(perf,average_score = round((math_score+reading_score+writing_score)/3))

new_perf$average_score

head(new_perf)

View(new_perf)

#Answer 1
agg_math_score = aggregate(new_perf$math_score, list(perf$group), FUN=mean)

colnames(agg_math_score) <- c('group','avg_math_score')

agg_math_score

agg_reading_score = aggregate(new_perf$reading_score, list(perf$group), FUN=mean)

colnames(agg_reading_score) <- c('group','avg_reading_score')

agg_reading_score

agg_writing_score = aggregate(new_perf$writing_score, list(perf$group), FUN=mean)

colnames(agg_writing_score) <- c('group','avg_writing_score')

agg_writing_score

agg_average_score = aggregate(new_perf$average_score, list(perf$group), FUN=mean)

colnames(agg_average_score) <- c('group','avg_student_performance')

agg_average_score

dev.off(dev.list()["RStudioGD"]) 

p1 = ggplot(agg_math_score, aes(x=group, y=avg_math_score, fill=group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Average_Math_Score")

p2 = ggplot(agg_reading_score, aes(x=group, y=avg_reading_score,fill=group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Average_Reading_Score")

p3 = ggplot(agg_writing_score, aes(x=group, y=avg_writing_score,fill=group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Average_Writing_Score")

p4 = ggplot(agg_average_score, aes(x=group, y=avg_student_performance,fill=group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Average_Student_Performance")

p1
p2
p3
p4
plot_grid(p1,p2,p3,p4,labels="AUTO")

#Answer 2

dev.off(dev.list()["RStudioGD"]) 


grp_avg_bar = perf %>% group_by(group,gender) %>%
  summarise(avg_math = mean(math_score))  

grp_avg_bar

ggplot(grp_avg_bar,(aes(x=group,y=avg_math,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")


#Answer 3

total_perf = mutate(perf,total_marks = (math_score+reading_score+writing_score))

total_perf$total_marks

head(total_perf)

grp3_total = total_perf %>% group_by(group,gender) %>%
  summarise(sum_total = sum(total_marks))  

grp3_total

ggplot(grp3_total,(aes(x=group,y=sum_total,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")

sum_box=ggplot(total_perf, mapping=aes(x=group, y=total_marks))+geom_boxplot()

sum_box

sum_box=ggplot(total_perf, mapping=aes(x=group, y=total_marks, fill=gender))+geom_boxplot()

sum_box

