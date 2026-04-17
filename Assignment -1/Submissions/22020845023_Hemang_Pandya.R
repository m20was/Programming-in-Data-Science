library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)

S_p = read_excel("D:/Study/PDS/input/StudentsPerformance.xlsx")

head(S_p)
tail(S_p)
summary(S_p)
View(S_p)
str(S_p)
nrow(S_p)
print(S_p)


S_p %>% filter(is.na(group))
S_p %>% filter(is.na(gender))
S_p %>% filter(group == 'group D' & reading_score >= 20)

new_S_p = mutate(S_p,average_score = round((math_score+reading_score+writing_score)/3))
new_S_p$average_score
head(new_S_p)
summary(new_S_p)
View(new_S_p)

#Analyze the performance of one of the groups

avg_math_performance = aggregate(new_S_p$math_score, list(S_p$group), FUN=mean)
colnames(avg_math_performance) <- c('group','avg_math_performance')
avg_math_performance

avg_reading_performance = aggregate(new_S_p$reading_score, list(S_p$group), FUN=mean)
colnames(avg_reading_performance) <- c('group','avg_reading_performance')
avg_reading_performance

avg_writing_performance = aggregate(new_S_p$writing_score, list(S_p$group), FUN=mean)
colnames(avg_writing_performance) <- c('group','avg_writing_performance')
avg_writing_performance

Tot_average_performance = aggregate(new_S_p$average_score, list(S_p$group), FUN=mean)
colnames(Tot_average_performance) <- c('group','Tot_average_performance')
Tot_average_performance

dev.off(dev.list()["RStudioGD"]) 

ggplot(avg_math_performance, aes(x=group, y=avg_math_performance, fill=group)) + 
  geom_bar(stat="identity") +  labs(x="Groups", y="avg_math_performance")

ggplot(avg_reading_performance, aes(x=group, y=avg_reading_performance,fill=group)) + 
  geom_bar(stat="identity") + labs(x="Groups", y="avg_reading_performance")

ggplot(avg_writing_performance, aes(x=group, y=avg_writing_performance,fill=group)) + 
  geom_bar(stat="identity") + labs(x="Groups", y="avg_writing_performance")

#Analyze the gender performance in Math for each group.

grp_avg = S_p %>% group_by(group,gender) %>%
  summarise(avg_math = mean(math_score))  
grp_avg

ggplot(grp_avg,(aes(x=group,y=avg_math,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")


# Add a new column “total mark” and analyze the group performance with respect to Total Marks

total_marks = mutate(S_p,total_score = (math_score+reading_score+writing_score))
total_marks$total_score
head(total_marks)

grp2_max = total_marks %>% group_by(group,gender) %>%
  summarise(sum_total = sum(total_score))  
grp2_max

ggplot(grp2_max,(aes(x=group,y=sum_total,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")
ggplot(total_marks,(aes(x=group,y=total_score,fill=gender))) +
  geom_bar(stat = "identity",position = )

