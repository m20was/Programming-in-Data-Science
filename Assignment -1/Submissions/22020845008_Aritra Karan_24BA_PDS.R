library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)

classtally = read_excel("/Users/91833/Desktop/r programming scripts/aritrakaran.xlsx")

View(classtally)

head(classtally)

tail(classtally)

summary(classtally)

nrow(classtally)

str(classtally)

print(classtally)

grp_count = classtally %>% count(group)
grp_count

classtally %>% filter(is.na(group))
classtally %>% filter(is.na(gender))
classtally %>% filter(gender=='female' & group=='group c')

new_classtally = mutate(classtally, avg=round((math_score+reading_score+writing_score)/3,4))
new_classtally$avg
head(new_classtally)

View(new_classtally)

# 1

agg_math_score = aggregate(new_classtally$math_score, list(classtally$group), FUN=mean)
colnames(agg_math_score) <- c('group','avg_math_score')
agg_math_score

agg_reading_score = aggregate(new_classtally$reading_score, list(classtally$group), FUN=mean)
colnames(agg_reading_score) <- c('group','avg_reading_score')
agg_reading_score

agg_writing_score = aggregate(new_classtally$writing_score, list(classtally$group), FUN=mean)
colnames(agg_writing_score) <- c('group','avg_writing_score')
agg_writing_score

agg_average_score = aggregate(new_classtally$average_score, list(classtally$group), FUN=mean)
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

# 2

grp2_avg = classtally %>% group_by(group,gender) %>%
  summarise(avg_math = mean(math_score))  
grp2_avg

ggplot(grp2_avg,(aes(x=group,y=avg_math,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")


# 3

total_classtally = mutate(classtally,total_score = (math_score+reading_score+writing_score))
total_classtally$total_score
head(total_classtally)

grp3_total = total_classtally %>% group_by(group,gender) %>%
  summarise(sum_total = sum(total_score))  
grp3_total

ggplot(grp3_total,(aes(x=group,y=sum_total,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")

total_box=ggplot(total_classtally, mapping=aes(x=group, y=total_score))+geom_boxplot()
total_box

total_box=ggplot(total_classtally, mapping=aes(x=group, y=total_score, fill=gender))+geom_boxplot()
total_box

