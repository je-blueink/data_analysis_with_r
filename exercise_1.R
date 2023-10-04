#---- 1 ----
install.packages("datarium")
library(datarium)
library(ggplot2)

head(jobsatisfaction)
summary(jobsatisfaction)

#1-1
ggplot(data=jobsatisfaction) +
  geom_boxplot(aes(x=gender, y=score)) + 
  labs(title="202235-368052")

#1-2
tapply(jobsatisfaction$score, jobsatisfaction$gender, mean)

#1-3
t.test(score~gender, data = jobsatisfaction)

#1-4
oneway.test(score~education_level, data=jobsatisfaction)

#1-5
library(dplyr)
job2 <- jobsatisfaction %>% filter(score >= 9)
summary(job2)
  
#1-6
ggplot(data = jobsatisfaction) +
  geom_histogram(aes(x=score), breaks=4:10, 
                 color="blue", fill="orange") +
  labs(title="202235-368052")

#1-7
ggplot(data = jobsatisfaction) +
  geom_histogram(aes(x=score), breaks=4:10, 
                 color="blue", fill="orange") +
  labs(title="202235-368052")+
  facet_wrap(vars(education_level))

#---- 2 ----
head(anxiety)
summary(anxiety)

#2-1
ggplot(data=anxiety)+
  geom_point(aes(x=t1, y=t2))+
  labs(title="202235-368052")

#2-2
cor.test(anxiety$t1, anxiety$t2)

#2-3
obj <- lm(t2~t1, data = anxiety)
summary(obj)

help("oneway.test")
help(lm)