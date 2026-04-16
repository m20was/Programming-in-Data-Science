library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)

StudPerf = read_excel("D:/SIBM/SIBM Sem 1/Programming for Data Sciences/Inputs/StudentsPerformance.xlsx")

View(StudPerf)

head(StudPerf)

tail(StudPerf)

summary(StudPerf)

nrow(StudPerf)

str(StudPerf)

print(StudPerf)

StudPerf %>% filter(is.na(group))

StudPerf %>% filter(is.na(gender))

StudPerf %>% filter(gender=='male' & group == 'group B')

StudPerf %>% filter(group == 'group B' & math_score == 40)

new_StudPerf = mutate(StudPerf,average_score = round((math_score+reading_score+writing_score)/3))
new_StudPerf$average_score
head(new_StudPerf)

View(new_StudPerf)

#ANS 1
grp_math_score = StudPerf %>% group_by(group) %>%
  summarise(sum_math_score = sum(math_score))  
grp_math_score

grp_reading_score = StudPerf %>% group_by(group) %>%
  summarise(sum_reading_score = sum(reading_score))  
grp_reading_score

grp_writing_score = StudPerf %>% group_by(group) %>%
  summarise(sum_writing_score = sum(writing_score))  
grp_writing_score

grp_average_score = new_StudPerf %>% group_by(group) %>%
  summarise(sum_average_score = sum(average_score))  
grp_average_score

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

#ANS 2
grp2_sum = StudPerf %>% group_by(group,gender) %>%
  summarise(sum_math = sum(math_score))  
grp2_sum

grp2_avg = StudPerf %>% group_by(group,gender) %>%
  summarise(avg_math = mean(math_score))  
grp2_avg

ggplot(grp2_sum,(aes(x=group,y=sum_math,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")

ggplot(grp2_avg,(aes(x=group,y=avg_math,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")


#ANS 3

total_StudPerf = mutate(StudPerf,total_score = (math_score+reading_score+writing_score))
total_StudPerf$total_score
head(total_StudPerf)

grp3_total = total_StudPerf %>% group_by(group,gender) %>%
  summarise(sum_total = sum(total_score))  
grp3_total

ggplot(grp3_total,(aes(x=group,y=sum_total,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")

total_box=ggplot(total_StudPerf, mapping=aes(x=group, y=total_score))+geom_boxplot()
total_box

total_box=ggplot(total_StudPerf, mapping=aes(x=group, y=total_score, fill=gender))+geom_boxplot()
total_box

#extra
Group_A_Count = StudPerf %>% count(group)
Group_A_Count

mean_math_score = aggregate(StudPerf$math_score, list(StudPerf$group), FUN = mean)
colnames(mean_math_score) = c('group','mean')
mean_math_score

med_math_score = aggregate(StudPerf$math_score, list(StudPerf$group), FUN = median)
colnames(med_math_score) = c('group','median')
med_math_score


