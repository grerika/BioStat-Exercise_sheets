## 
## I.1. Egy kezelés során szükségessé vált annak ellenőrzése, hogy az milyen hatással van a vérnyomásra. 
## n = 9 személyt megmérve a következő értékeket kapták: 
# 182,00  152,00  178,00  157,00  194,00  163,00  144,00  114,00  174,00
#  Az átlag = 162 Hgmm volt, a standard deviáció SD = 23,92 lett.
d11 = c(182.00,152.00,178.00,157.00,194.00,163.00,144.00,114.00,174.00)
mean(d11)
sd(d11)
length(d11)
se=sd(d11)/sqrt(length(d11))
se
t.test(d11)
tvalue = (mean(d11)-130)/se

## II.1. 
before = c(7.43,7.39,7.37,7.43,7.39,7.36,7.38,7.39,7.34,7.32,7.40,7.32,7.42,7.42,7.37,7.37,7.39,7.43)
after  = c(7.43,7.39,7.38,7.42,7.39,7.41,7.38,7.39,7.41,7.35,7.39,7.33,7.39,7.40,7.36,7.39,7.37,7.48)
mean(before)
sd(before)
mean(after)
sd(after)

diff = before-after
mean(diff)
sd(diff)

t.test(before,after,paired=TRUE)

## II.2.
before2 = c(7.42,7.36,7.40,7.43,7.38,7.32,7.37,7.36,7.34,7.31,7.34,7.37,7.42,7.42,7.46,7.37,7.45,7.42,7.42,7.41)
after2  = c(7.46,7.43,7.46,7.48,7.42,7.45,7.46,7.48,7.45,7.37,7.47,7.43,7.48,7.43,7.51,7.41,7.48,7.44,7.37,7.45)
diff2 = before2-after2

t.test(before2,after2,paired=TRUE)

## II.5 
setwd("~/Work/Education/Biostatisztika/2014/Monitol/Adatok/R")
data = read.csv("befafter.csv")
attach(data)
diff3 = (data$BEFORE-data$AFTER)
t.test(diff3)
plot(diff3)
mean(BEFORE)
sd(BEFORE)
mean(AFTER)
sd(AFTER)

t.test(BEFORE,AFTER,paired=TRUE)
qt(0.975,length(BEFORE)-1)

## II.6.
setwd("/home/grus/Downloads")
adat2 = read.csv("orvkerd2015.csv")
attach(adat2)
str(adat2)
boxplot(tomeg)
boxplot(magassag)

t.test(tomeg1-tomeg)
boxplot(tomeg~nem)


boxplot(magassag~nem)
summary(magassag[nem=="fi\xfa"])
sd(magassag[nem=="fi\xfa"])
summary(magassag[nem=="l\xe1ny"])
sd(magassag[nem=="l\xe1ny"])

t.test(magassag[nem=="l\xe1ny"],magassag[nem=="fi\xfa"])
var.test(magassag[nem=="l\xe1ny"],magassag[nem=="fi\xfa"])
### III.1. Two-sample t-test (angol)
control   = c(170,160,150,150,180,170,160,160)
treatment = c(120,130,120,130,110,130,140,150,130,120)
n = length(control);m = length(treatment);
m1=mean(control);sd1=sd(control);var1=var(control);
m2=mean(treatment);sd2=sd(treatment);var2=var(treatment);
sp = (n-1)*sd1^2+(m-1)*sd2^2
sp = sp/(n+m-2)
t =  (m1-m2)/sqrt(sp)*sqrt(n*m/(n+m))
sprintf("Length: %2d and %2d, Means: %.2f and %.2f",n,m,m1,m2,sd1,sd2)
sprintf("SDs:  %.2f and %.2f",sd1,sd2)
sprintf("sp= %.4f t=%.4f",sp,t)

hist(control)
hist(treatment)
boxplot(control,treatment)
t.test(control,treatment)
t.test(control,treatment,var.equal = TRUE)

var.test(control,treatment)
var1/var2
## III.4.
