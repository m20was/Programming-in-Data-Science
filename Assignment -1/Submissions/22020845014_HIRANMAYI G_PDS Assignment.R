library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)

ds= read.csv("C:/Users/Hiranmayi Guntur/Downloads/StudentsPerformance.csv")

View(ds)
head(ds)
tail(ds)
nrow(ds)
summary(ds)
str(ds)
print(ds)

grp1 = ds %>% count(group)
grp1

ds %>% filter(is.na(group))
ds %>% filter(is.na(gender))
ds %>% filter(gender=='female' & group=='group c')

ds1 = mutate(ds, avg=round((math_score+reading_score+writing_score)/3,4))
ds1$avg
head(ds1)

#Analyze the performance of one of the groups

grp_m = ds %>% group_by(group) %>%
  summarise(sum_m = sum(math_score))  
grp_m

grp_r = ds %>% group_by(group) %>%
  summarise(sum_r = sum(reading_score))  
grp_r

grp_w = ds1 %>% group_by(group) %>%
  summarise(sum_w = sum(writing_score))  
grp_w

grp_avg = ds1 %>% group_by(group) %>%
  summarise(sum_avg = sum(avg))  
grp_avg


dev.off(dev.list()["RStudioGD"]) 

p1 = ggplot(grp_m, aes(x=group, y=sum_m,fill=group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Total Math Score")
p1

p2 = ggplot(grp_r, aes(x=group, y=sum_r,fill=group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Total Reading Score")
p2

p3 = ggplot(grp_w, aes(x=group, y=sum_w,fill=group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Total Writing Score")
p3

p4 = ggplot(grp_avg, aes(x=group, y=sum_avg,fill=group)) + geom_bar(stat="identity") + 
  labs(x="Groups", y="Total Average Score")
p4

plot_grid(p1,p2,p3,p4,labels="AUTO")

#Analyze the gender performance in Math for each group
math_perf = ds %>% group_by(group,gender) %>%
  summarise(avg_math = mean(math_score))  
math_perf

ggplot(math_perf,(aes(x=group,y=avg_math,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")

#Add a new column “total mark” and analyze the group performance with respect to Total Marks
total = mutate(ds,total_score = (math_score+reading_score+writing_score))
total$total_score
head(total)

ds3 = total %>% group_by(group,gender) %>%
  summarise(sum_total = sum(total_score))  
ds3

ggplot(ds3,(aes(x=group,y=sum_total,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")

  p5=ggplot(ds3, mapping=aes(x=group, y=sum_total))+geom_boxplot()
p5

p6=ggplot(ds3, mapping=aes(x=group, y=sum_total, fill=gender))+geom_boxplot()
p6
