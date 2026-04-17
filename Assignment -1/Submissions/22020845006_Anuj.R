library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)

perform =read_excel("C:/Users/anujg/OneDrive/Desktop/studentsperformance.xlsx")

View(perform)

head(perform)

tail(perform)

summary(perform)

nrow(perform)

str(perform)

print(perform)

grp_count = perform %>% count(group)
grp_count

perform %>% filter(is.na(group))

perform %>% filter(is.na(gender))

perform %>% filter(gender=='male' & group == 'group B')

perform %>% filter(group == 'group B' & math_score == 40)

new_perform = mutate(perform,average_score = round((math_score+reading_score+writing_score)/3))
new_perform$average_score
head(new_perform)

View(new_perform)

#ANS 1

agg_math_score = aggregate(new_perform$math_score, list(perform$group), FUN=mean)
colnames(agg_math_score) <- c('group','avg_math_score')
agg_math_score

agg_reading_score = aggregate(new_perform$reading_score, list(perform$group), FUN=mean)
colnames(agg_reading_score) <- c('group','avg_reading_score')
agg_reading_score

agg_writing_score = aggregate(new_perform$writing_score, list(perform$group), FUN=mean)
colnames(agg_writing_score) <- c('group','avg_writing_score')
agg_writing_score

agg_average_score = aggregate(new_perform$average_score, list(perform$group), FUN=mean)
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

grp2_avg = perform %>% group_by(group,gender) %>%
  summarise(avg_math = mean(math_score))  
grp2_avg

ggplot(grp2_avg,(aes(x=group,y=avg_math,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")


#ANS 3

total_perform = mutate(perform,total_score = (math_score+reading_score+writing_score))
total_perform$total_score
head(total_perform)

grp3_total = total_perform %>% group_by(group,gender) %>%
  summarise(sum_total = sum(total_score))  
grp3_total

ggplot(grp3_total,(aes(x=group,y=sum_total,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")

total_box=ggplot(total_perform, mapping=aes(x=group, y=total_score))+geom_boxplot()
total_box

total_box=ggplot(total_perform, mapping=aes(x=group, y=total_score, fill=gender))+geom_boxplot()
total_box


