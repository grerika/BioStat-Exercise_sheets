adat=read.csv2("/home/grus/Work/Education/Biostatisztika/Dolgozatok/Plots/quest2016.csv")
attach(adat)

# 3.1 mass
variable = mass
df = length(variable)-1
# alpha = 5%
alpha=0.05
talpha5 = qt(1-alpha/2,df)
talpha5
# alpha = 1%
alpha=0.01
talpha1 = qt(1-alpha/2,df)
talpha1


length(variable)
m = mean(variable)
sd(variable)
SE = sd(variable)/sqrt(length(variable))
SE


m-talpha5*SE;m+talpha5*SE
m-talpha1*SE;m+talpha1*SE

# 3.2 height
variable = height
length(variable)
m = mean(variable)
sd(variable)
SE = sd(variable)/sqrt(length(variable))
SE

m-talpha5*SE;m+talpha5*SE
m-talpha1*SE;m+talpha1*SE

