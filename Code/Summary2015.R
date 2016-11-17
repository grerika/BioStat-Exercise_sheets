setwd("~/Work/Education/Biostatisztika")
d1 = read.csv("quest2015.csv")
attach(d1)
str(d1)

cor.test(mass,ideal_mass)
lm(ideal_mass~mass)

chisq.test(eye,gender)
chisq.test(eye,gender)$observed
chisq.test(eye,gender)$expected

setwd("~/Work/Education/Biostatisztika/2014/Monitol/Adatok/R")
d2 = read.csv("QUEST_2013.csv")
attach(d2)
str(d2)
cor.test(mass,idealmass)
lm(idealmass~mass)

mean(mass);sd(mass);length(mass)
mean(mass3);sd(mass3);length(mass3)
t.test(mass,mass3,paired=TRUE)
var.test(mass,mass3)

chisq.test(eye,gender)
chisq.test(eye,gender)$observed
chisq.test(eye,gender)$expected

