
cor_stat <- function (n,r ){
  t = r*sqrt(n-2)/sqrt(1-r^2)
  return(t)
}
## 1
setwd("/home/grus/Work/Education/Biostatisztika/Feladatsorok/Plots/")
d  = read.csv("/home/grus/Work/Education/Biostatisztika/Archive/Adatok/ORVKERD2011.csv")#orvkerd.csv
attach(d)
str(d)

dep = tomeg
indep = magassag
summary(dep)
summary(indep)

pdf("Korrelacio-1.pdf")
plot(indep,dep,xlab="height",ylab="body mass")
dev.off()
plot(indep,dep,xlab="height",ylab="body mass")
R = cor(indep,dep);R
cor.test(indep,dep)

fit = lm ( dep ~ indep )   ## a regresszios egyenes egyenlete
fit
summary ( fit )
summary(fit)$r.squared 
R^2


## 2. Egy brit felmÃ©rÃ©sben a dohÃ¡nyra illetve alkoholra 
## tÃ¶rtÃ©nÅ‘ kiadÃ¡sok kÃ¶zÃ¶tti Ã¶sszefÃ¼ggÃ©st vizsgÃ¡ltÃ¡k Nagy-Britannia 10 kÃ¼lÃ¶nbÃ¶zÅ‘ tartomÃ¡nyÃ¡ban. 
d2 = read.csv("alcohol.csv")
attach(d2)
str(d2)
dep = alcohol
indep = tobacco
pdf("Korrelacio-2.pdf")
plot(indep,dep,xlab="tobacco",ylab="alcohol")
dev.off()
plot(indep,dep,xlab="tobacco",ylab="alcohol")
abline(lm(dep~indep), col="red", lwd=3)   
cor.test(indep,dep)

#3.1 
n=5
r=0.8
cor_stat(n,r)

n=500;
r=0.2
cor_stat(n,r)

n=15
r=-0.8
cor_stat(n,r)


#4.1 Vizsgáljuk meg a testmagasság és a testtömeg közötti lineáris kapcsolatot! Készítsünk ábrát
adat=read.csv2("/home/grus/Desktop/Aktualis/Moni/HUN_Feladatok_Moni/Adatbazisok/orvkerd2016.csv")
fit = lm(adat$tomeg~adat$magassag)
plot(adat$magassag,adat$tomeg)
abline(fit,col="red")
cor.test(adat$magassag,adat$tomeg)
fit
summary(fit)


# 4.2 Olvassuk be az antropometria.csv adatfájlt! Vizsgáljuk meg a testmagasság elsõ és második mérése közötti kapcsolatot!
adat=read.csv("/home/grus/Desktop/Aktualis/Moni/HUN_Feladatok_Moni/Adatbazisok/antropometrics.csv")
fit = lm(adat$weight1~adat$weight2)
plot(adat$weight2,adat$weight1,xlab="weight1",ylab="weight2")
abline(fit,col="red")
cor.test(adat$weight2,adat$weight1)
fit
summary(fit)


# 4.3 Olvassuk be az antropometria.csv adatfájlt! Vizsgáljuk ebben a fájlban a testmagasság és a csípõkörfogat közötti kapcsolatot!
fit = lm(adat$height1~adat$hip1)
plot(adat$hip1,adat$height1,xlab="height1",ylab="hip1")
abline(fit,col="red")
cor.test(adat$hip1,adat$height1)
fit
summary(fit)

