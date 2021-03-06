
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


## 2. Egy brit felmérésben a dohányra illetve alkoholra 
## történő kiadások közötti összefüggést vizsgálták Nagy-Britannia 10 különböző tartományában. 
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


#4.1 Vizsg�ljuk meg a testmagass�g �s a testt�meg k�z�tti line�ris kapcsolatot! K�sz�ts�nk �br�t
adat=read.csv2("/home/grus/Desktop/Aktualis/Moni/HUN_Feladatok_Moni/Adatbazisok/orvkerd2016.csv")
fit = lm(adat$tomeg~adat$magassag)
plot(adat$magassag,adat$tomeg)
abline(fit,col="red")
cor.test(adat$magassag,adat$tomeg)
fit
summary(fit)


# 4.2 Olvassuk be az antropometria.csv adatf�jlt! Vizsg�ljuk meg a testmagass�g els� �s m�sodik m�r�se k�z�tti kapcsolatot!
adat=read.csv("/home/grus/Desktop/Aktualis/Moni/HUN_Feladatok_Moni/Adatbazisok/antropometrics.csv")
fit = lm(adat$weight1~adat$weight2)
plot(adat$weight2,adat$weight1,xlab="weight1",ylab="weight2")
abline(fit,col="red")
cor.test(adat$weight2,adat$weight1)
fit
summary(fit)


# 4.3 Olvassuk be az antropometria.csv adatf�jlt! Vizsg�ljuk ebben a f�jlban a testmagass�g �s a cs�p�k�rfogat k�z�tti kapcsolatot!
fit = lm(adat$height1~adat$hip1)
plot(adat$hip1,adat$height1,xlab="height1",ylab="hip1")
abline(fit,col="red")
cor.test(adat$hip1,adat$height1)
fit
summary(fit)

