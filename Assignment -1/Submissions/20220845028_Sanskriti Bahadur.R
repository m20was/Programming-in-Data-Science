library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)


students = read_excel("D:/SIBM Bengaluru - Studies/Programming for Data Sciences/Assignment - 1/StudentsPerformance.xlsx")

View(students)
head(students)
tail(students)
nrow(students)
summary(students)
str(students)
print(students)


grp_count = students %>% count(group)
grp_count

summarise(students,med = median(math_score))
summarise(students,med = median(reading_score))
summarise(students, med = median (writing_score))          

Group_A_Count = students %>% count(group)
Group_A_Count



students %>% filter(is.na(group))
students %>% filter(is.na(gender))
students %>% filter(gender=='male' & group =='group B')

students %>% filter(group == 'group B' & math_score == 90)


#Analysing the performance of one group
grp_math_score = students %>% group_by(group) %>%
  summarise(sum_math_score = sum(math_score))  
grp_math_score

grp_reading_score = students %>% group_by(group) %>%
  summarise(sum_reading_score = sum(reading_score))  
grp_reading_score

grp_writing_score = students %>% group_by(group) %>%
  summarise(sum_writing_score = sum(writing_score))  
grp_writing_score

grp_average_score = new_students %>% group_by(group) %>%
  summarise(sum_average_score = sum(average_score))  
grp_average_score

agg_math_score = aggregate(new_students$math_score, list(students$group), FUN=mean)
colnames(agg_math_score) <- c('group','avg_math_score')
agg_math_score

agg_reading_score = aggregate(new_students$reading_score, list(students$group), FUN=mean)
colnames(agg_reading_score) <- c('group','avg_reading_score')
agg_reading_score

agg_writing_score = aggregate(new_students$writing_score, list(students$group), FUN=mean)
colnames(agg_writing_score) <- c('group','avg_writing_score')
agg_writing_score

agg_average_score = aggregate(new_students$average_score, list(students$group), FUN=mean)
colnames(agg_average_score) <- c('group','avg_student_performance')
agg_average_score


dev.off(dev.list()["RStudioGD"]) 

p1 = ggplot(grp_math_score, aes(x=group, y=sum_math_score,fill=group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Total_Math_Score")
p1

p2 = ggplot(grp_reading_score, aes(x=group, y=sum_reading_score,fill=group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Total_Reading_Score")
p2

p3 = ggplot(grp_writing_score, aes(x=group, y=sum_writing_score,fill=group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Total_Writing_Score")
p3

p4 = ggplot(grp_average_score, aes(x=group, y=sum_average_score,fill=group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Total_Average_Score")
p4

plot_grid(p1,p2,p3,p4,labels="AUTO")

# Analysing gender performance in math for each group
grp2_sum = students %>% group_by(group,gender) %>%
  summarise(sum_math = sum(math_score))  
grp2_sum

grp2_avg = students %>% group_by(group,gender) %>%
  summarise(avg_math = mean(math_score))  
grp2_avg

ggplot(grp2_sum,(aes(x=group,y=sum_math,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")

ggplot(grp2_avg,(aes(x=group,y=avg_math,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")


#Add a new column "Total Mark" and analyse the group performance with respect to total marks

total_students = mutate(students,total_score = (math_score+reading_score+writing_score))
total_students$total_score
head(total_students)

grp3_total = total_students %>% group_by(group,gender) %>%
  summarise(sum_total = sum(total_score))  
grp3_total

ggplot(grp3_total,(aes(x=group,y=sum_total,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")

total_box=ggplot(total_students, mapping=aes(x=group, y=total_score))+geom_boxplot()
total_box

total_box=ggplot(total_students, mapping=aes(x=group, y=total_score, fill=gender))+geom_boxplot()
total_box

#Pie Chart
grpby1 = students %>% group_by(total_score) %>%
  summarise(sum_students = round(sum(total_score)/sum(students$total_score) *100,2))

ggplot(grpby1, aes(x = "", y = sum_total_score, fill = group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(sum_total_score, "%")), 
            position = position_stack(vjust=0.5))+
  labs(x = NULL, y = NULL) +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())  +
  ggtitle("Pie Chart for Group Vs Total Score")


