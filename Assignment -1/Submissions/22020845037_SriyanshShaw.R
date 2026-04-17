library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)

StudpPrf = read_excel("D:/Downloads/PDS - R/StudentsPerformance.xlsx")

View(StudpPrf)

head(StudpPrf)

tail(StudpPrf)

summary(StudpPrf)

nrow(StudpPrf)

str(StudpPrf)

print(StudpPrf)

grp_count = StudpPrf %>% count(group)
grp_count

StudpPrf %>% filter(is.na(group))

StudpPrf %>% filter(is.na(gender))

StudpPrf %>% filter(gender=='male' & group == 'group B')

StudpPrf %>% filter(group == 'group B' & math_score == 40)

New_StudpPrf = mutate(StudpPrf,average_score = round((math_score+reading_score+writing_score)/3))
New_StudpPrf$average_score
head(New_StudpPrf)

View(New_StudpPrf)

#ANSWER 1

agg_math_score = aggregate(New_StudpPrf$math_score, list(StudpPrf$group), FUN=mean)
colnames(agg_math_score) <- c('group','avg_math_score')
agg_math_score

agg_reading_score = aggregate(New_StudpPrf$reading_score, list(StudpPrf$group), FUN=mean)
colnames(agg_reading_score) <- c('group','avg_reading_score')
agg_reading_score

agg_writing_score = aggregate(New_StudpPrf$writing_score, list(StudpPrf$group), FUN=mean)
colnames(agg_writing_score) <- c('group','avg_writing_score')
agg_writing_score

agg_average_score = aggregate(New_StudpPrf$average_score, list(StudpPrf$group), FUN=mean)
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

#ANSWER 2

grp2_avg = StudpPrf %>% group_by(group,gender) %>%
  summarise(avg_math = mean(math_score))  
grp2_avg

ggplot(grp2_avg,(aes(x=group,y=avg_math,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")


#ANSWER 3

Total_StudpPrf = mutate(StudpPrf,Total_Score = (math_score+reading_score+writing_score))
Total_StudpPrf$Total_Score
head(Total_StudpPrf)

grp3_total = Total_StudpPrf %>% group_by(group,gender) %>%
  summarise(sum_total = sum(Total_Score))  
grp3_total

ggplot(grp3_total,(aes(x=group,y=sum_total,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")

total_box=ggplot(Total_StudpPrf, mapping=aes(x=group, y=Total_Score))+geom_boxplot()
total_box

total_box=ggplot(Total_StudpPrf, mapping=aes(x=group, y=Total_Score, fill=gender))+geom_boxplot()
total_box

