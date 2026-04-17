library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)

stuper = read_excel("/Users/shivam/Desktop/pds_midterm/StudentsPerformance.xlsx")

View(stuper)

head(stuper)

tail(stuper)

summary(stuper)

nrow(stuper)

str(stuper)

print(stuper)

grp_count = stuper %>% count(group)
grp_count

stuper %>% filter(is.na(group))

stuper %>% filter(is.na(gender))

stuper %>% filter(gender=='male' & group == 'group B')

stuper %>% filter(group == 'group B' & math_score == 40)

new_stuper = mutate(stuper,average_score = round((math_score+reading_score+writing_score)/3))
new_stuper$average_score
head(new_stuper)

View(new_stuper)

#ANS 1

agg_math_score = aggregate(new_stuper$math_score, list(stuper$group), FUN=mean)
colnames(agg_math_score) <- c('group','avg_math_score')
agg_math_score

agg_reading_score = aggregate(new_stuper$reading_score, list(stuper$group), FUN=mean)
colnames(agg_reading_score) <- c('group','avg_reading_score')
agg_reading_score

agg_writing_score = aggregate(new_stuper$writing_score, list(stuper$group), FUN=mean)
colnames(agg_writing_score) <- c('group','avg_writing_score')
agg_writing_score

agg_average_score = aggregate(new_stuper$average_score, list(stuper$group), FUN=mean)
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

grp2_avg = stuper %>% group_by(group,gender) %>%
  summarise(avg_math = mean(math_score))  
grp2_avg

ggplot(grp2_avg,(aes(x=group,y=avg_math,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")


#ANS 3

total_stuper = mutate(stuper,total_score = (math_score+reading_score+writing_score))
total_stuper$total_score
head(total_stuper)

grp3_total = total_stuper %>% group_by(group,gender) %>%
  summarise(sum_total = sum(total_score))  
grp3_total

ggplot(grp3_total,(aes(x=group,y=sum_total,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")

total_box=ggplot(total_stuper, mapping=aes(x=group, y=total_score))+geom_boxplot()
total_box

total_box=ggplot(total_stuper, mapping=aes(x=group, y=total_score, fill=gender))+geom_boxplot()
total_box

