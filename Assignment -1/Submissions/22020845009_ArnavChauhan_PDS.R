library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)

Students_Perf = read_excel("C:/Users/ADMIN/OneDrive/Desktop/SIBMB/SEM 1/PDS/ASSIGNMENTS/StudentsPerformance.xlsx")

View(Students_Perf)

head(Students_Perf)

tail(Students_Perf)

summary(Students_Perf)

nrow(Students_Perf)

str(Students_Perf)

print(Students_Perf)

Grp_count = Students_Perf %>% count(group)
Grp_count

Students_Perf %>% filter(is.na(group))

Students_Perf %>% filter(is.na(gender))

Students_Perf %>% filter(gender=='male' & group == 'group B')

Students_Perf %>% filter(group == 'group B' & math_score == 40)

New_Students_Perf = mutate(Students_Perf,average_score = round((math_score+reading_score+writing_score)/3))
New_Students_Perf$average_score
head(New_Students_Perf)

View(New_Students_Perf)


#ANSWER 1

Agg_math_score = aggregate(New_Students_Perf$math_score, list(Students_Perf$group), FUN=mean)
colnames(Agg_math_score) <- c('Group','Avg_math_score')
Agg_math_score

Agg_reading_score = aggregate(New_Students_Perf$reading_score, list(Students_Perf$group), FUN=mean)
colnames(Agg_reading_score) <- c('Group','Avg_reading_score')
Agg_reading_score

Agg_writing_score = aggregate(New_Students_Perf$writing_score, list(Students_Perf$group), FUN=mean)
colnames(Agg_writing_score) <- c('Group','Avg_writing_score')
Agg_writing_score

Agg_average_score = aggregate(New_Students_Perf$average_score, list(Students_Perf$group), FUN=mean)
colnames(Agg_average_score) <- c('Group','Avg_student_performance')
Agg_average_score

dev.off(dev.list()["RStudioGD"]) 

p1 = ggplot(Agg_math_score, aes(x=Group, y=Avg_math_score, fill=Group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Average_Math_Score")
p1

p2 = ggplot(Agg_reading_score, aes(x=Group, y=Avg_reading_score,fill=Group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Average_Reading_Score")
p2

p3 = ggplot(Agg_writing_score, aes(x=Group, y=Avg_writing_score,fill=Group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Average_Writing_Score")
p3

p4 = ggplot(Agg_average_score, aes(x=Group, y=Avg_student_performance,fill=Group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Average_Student_Performance")
p4

plot_grid(p1,p2,p3,p4,labels="AUTO")



#ANSWER 2

Grp2_avg = New_Students_Perf %>% group_by(Group,Gender) %>%
  summarise(Avg_math = mean(math_score))  
Grp2_avg

ggplot(Grp2_avg,(aes(x=group,y=Avg_math,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")



#ANSWER 3

Total_Students_Perf = mutate(Students_Perf,Total_Score = (math_score+reading_score+writing_score))
Total_Students_Perf$Total_Score
head(Total_Students_Perf)

Grp3_Total = Total_Students_Perf %>% group_by(group,gender) %>%
  summarise(Sum_Total = sum(Total_Score))  
Grp3_Total

ggplot(Grp3_Total,(aes(x=group,y=Sum_Total,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")

total_box=ggplot(Total_Students_Perf, mapping=aes(x=group, y=Total_Score))+geom_boxplot()
total_box

total_box=ggplot(Total_Students_Perf, mapping=aes(x=group, y=Total_Score, fill=gender))+geom_boxplot()
total_box