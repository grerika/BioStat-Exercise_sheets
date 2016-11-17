setwd("S:/R")
data = read.csv("QUEST2014.csv")
attach(data)
plot(weight,ideal_weight)
cor(weight,ideal_weight)
cor.test(weight,ideal_weight)
lm(weight~ideal_weight)

# Ex 2. Paired t-test
diff = weight-weight_was
qqnorm(diff);qqline(diff)
mean(weight)
mean(weight_was)
sd(weight)
sd(weight_was)
length(weight)
t.test(weight,weight_was,paired=TRUE)

# Exercise 3
female = age[gender=="Female"] 
male   = age[gender=="Male"] 
mean(female)
sd(female)
length(female)
mean(male,na.rm=TRUE)
sd(male,na.rm=TRUE)
length(male)
t.test(age~gender)
t.test(female,male)
var.test(female,male)

# Exercise 4 
observed = table(difficult,gender)
observed
chisq.test(difficult,gender)$expected
chisq.test(difficult,gender)

