
library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)


par(mfrow=c(1,1))

tips = read_excel("C:/Users/HP/Downloads/StudentsPerformance.xlsx")

# View the data
View(tips)
head(tips)
tips$day
tail(tips)
summary(tips)
nrow(tips)
str(tips)

colors = seq(612,656,by=3)


summarise(total_marks, med = median(mark_total))
grp_count = tips %>% count(group)
grp_count

#mutate - to add a new col
total_marks = mutate(tips,mark_total = (math_score + reading_score + writing_score))
total_marks$mark_total
View(total_marks)


avg_marks = mutate(total_marks,mark_avg = (mark_total/3))
avg_marks$mark_avg
View(avg_marks)


agg_math = aggregate(avg_marks$math_score, list(tips$group), FUN=mean)
colnames(agg_math) <- c('group','avg_math_score')
agg_math

agg_reading = aggregate(avg_marks$reading_score, list(tips$group), FUN=mean)
colnames(agg_reading) <- c('group','avg_reading_score')
agg_reading

agg_writing = aggregate(avg_marks$writing_score, list(tips$group), FUN=mean)
colnames(agg_writing) <- c('group','avg_writing_score')
agg_writing

agg_average = aggregate(avg_marks$mark_avg, list(tips$group), FUN=mean)
colnames(agg_average) <- c('group','avg_student_performance')
agg_average

dev.off(dev.list()["RStudioGD"]) 

p1 = ggplot(agg_math, aes(x=group, y=avg_math_score, fill=group)) + geom_bar(stat="identity") + 
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
# arrange or order
marks_sort = arrange(avg_marks,mark_avg)
View(marks_sort)


dev.off(dev.list()["RStudioGD"]) 

ggplot(total_marks, aes(x=mark_total)) + 
  geom_freqpoly(binwidth=5,size=1)

p1=ggplot(total_marks, aes(x=mark_total,color=group)) + 
  geom_freqpoly(binwidth=5,size=1)

p2=ggplot(total_marks, aes(x=mark_total,color=gender)) + 
  geom_freqpoly(binwidth=5,size=1)



plot_grid(p1,p2,labels="AUTO")

ggplot(total_marks,(aes(x=group,fill=gender))) +
  geom_bar()

ggplot(total_marks,(aes(x=mark_total,fill=group))) +
  geom_bar()

ggplot(total_marks,(aes(x=group,fill=gender))) +
  geom_bar()




ggplot(tips,(aes(x=group,fill=gender))) +
  geom_bar()

ggplot(tips,(aes(x=math_score,fill=group))) +
  geom_bar()

ggplot(tips,(aes(x=group,fill=gender))) +
  geom_bar()



group_table = table(total_marks$group)
barplot(group_table,col=colors,main="Group Vs Total Marks")
group_table
barplot(tapply(total_marks$t=mark_total,tips$group,sum),col=colors,main="Gender Vs Total Tip Amt")




# Scatter plot
x=ggplot(total_marks, aes(x=mark_total, y=group,fill=gender)) +
  geom_point(size=2, shape=23)
x

plot_grid(x)


