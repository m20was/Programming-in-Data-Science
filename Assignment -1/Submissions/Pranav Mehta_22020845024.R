install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("cowplot")

library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)

Book1=read_excel("D:/Book1.xlsx")

View(Book1)
head(Book1)
Book1$day
tail(Book1)
summary(Book1)
nrow(Book1)
str(Book1)

grp_count = Book1 %>% count(group)
grp_count

Book1 %>% filter(is.na(group))
Book1 %>% filter(is.na(gender))
Book1 %>% filter(gender=='male' & group == 'group B')
Book1 %>% filter(group == 'group B' & math_score == 50)

new_Book1 = mutate(Book1 ,average_score = round((math_score+reading_score+writing_score)/3))
new_Book1$average_score
head(new_Book1)

View(new_Book1)

#Ans 1 

agg_math_score = aggregate(new_Book1$math_score, list(Book1$group), FUN=mean)
colnames(agg_math_score) <- c('group','avg_math_score')
agg_math_score

agg_reading_score = aggregate(new_Book1$reading_score, list(Book1$group), FUN=mean)
colnames(agg_reading_score) <- c('group','avg_reading_score')
agg_reading_score

agg_writing_score = aggregate(new_Book1$writing_score, list(Book1$group), FUN=mean)
colnames(agg_writing_score) <- c('group','avg_writing_score')
agg_writing_score

agg_average_score = aggregate(new_Book1$average_score, list(Book1$group), FUN=mean)
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

#Ans 2 

grp2_avg = Book1 %>% group_by(group,gender) %>%
  summarise(avg_math = mean(math_score))  
grp2_avg

ggplot(grp2_avg,(aes(x=group,y=avg_math,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")

#Ans 3

total_Book1 = mutate(Book1 ,total_score = (math_score+reading_score+writing_score))
total_Book1$total_score
head(total_Book1)

grp3_total = total_Book1 %>% group_by(group,gender) %>%
  summarise(sum_total = sum(total_score))  
grp3_total

ggplot(grp3_total,(aes(x=group,y=sum_total,fill=gender))) +
  geom_bar(stat = "identity",position = "dodge")

total_box=ggplot(total_Book1, mapping=aes(x=group, y=total_score))+geom_boxplot()
total_box

total_box=ggplot(total_Book1, mapping=aes(x=group, y=total_score, fill=gender))+geom_boxplot()
total_box
