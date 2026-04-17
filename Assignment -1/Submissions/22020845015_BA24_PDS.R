library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)

stuper = read_excel("C:/Users/Hp/Documents/PDS Project/StudentsPerformance.xlsx")

View(stuper)

head(stuper)

tail(stuper)

summary(stuper)

nrow(stuper)

str(stuper)

print(stuper)

stuper %>% filter(is.na(group))

stuper %>% filter(is.na(gender))

stuper %>% filter(gender=='male' & group == 'group B')

stuper %>% filter(group == 'group B' & math_score == 45)

new_stuper = mutate(stuper,average_score = round((math_score+reading_score+writing_score)/4))
new_stuper$average_score
head(new_stuper)

View(new_stuper)

################################################################################
#ANSWER 1

grp_math_score = stuper %>% group_by(group) %>%
  summarise(sum_math_score = sum(math_score))  
grp_math_score

grp_reading_score = stuper %>% group_by(group) %>%
  summarise(sum_reading_score = sum(reading_score))  
grp_reading_score

grp_writing_score = stuper %>% group_by(group) %>%
  summarise(sum_writing_score = sum(writing_score))  
grp_writing_score

grp_average_score = new_stuper %>% group_by(group) %>%
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



################################################################################
#ANSWER 2
grp2_sum = stuper %>% group_by(group,gender) %>%
  summarise(sum_math = sum(math_score))  
grp2_sum

grp2_avg = stuper %>% group_by(group,gender) %>%
  summarise(avg_math = mean(math_score))  
grp2_avg

ggplot(grp2_sum,(aes(x=group,y=sum_math,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")

ggplot(grp2_avg,(aes(x=group,y=avg_math,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")



################################################################################
#ANSWER 3

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

