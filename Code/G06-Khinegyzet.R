

# 1. Egy kísérletben azt vizsgálták, hogy a férfiak és a nők eltérő arányban vesznek-e részt rendszeres orvosi szűrővizsgálatokon.
megfigyelt = matrix(c(15,25,40,20),nrow=2)
megfigyelt
chisq.test(megfigyelt,correct=FALSE)
chisq.test(megfigyelt)
chisq.test(megfigyelt)$observed
chisq.test(megfigyelt)$expected

# 2. Két gyógyszer esetén figyelték egy bizonyos mellékhatás megjelenését. 
# A 60~páciens mindegyikét véletlenszerűen az A vagy a B gyógyszer valamelyikével kezelték.
megfigyelt = matrix(c(10,5,20,25),nrow=2)
megfigyelt
chisq.test(megfigyelt,correct=FALSE)
chisq.test(megfigyelt)
chisq.test(megfigyelt)$observed
chisq.test(megfigyelt)$expected


# 3.Egy bizonyos városban kb. 10\,000 szavazásra jogosult él. 
# Egy 500~főből álló véletlen minta alapján vizsgálták az életkor és a legutóbbi szavazáson való részvétel közötti kapcsolatot. 

megfigyelt = matrix(c(90,130,120,60,70,30),nrow=3)
megfigyelt
chisq.test(megfigyelt)
chisq.test(megfigyelt)$observed
chisq.test(megfigyelt)$expected


# 4. Egy tanulmányban azt vizsgálták, hogy az aszpirin hatással van-e a vérrögök kialakulására. 
# A 40 páciens mindegyike véletlenszerűen kapott placebót vagy aszpirint.

megfigyelt = matrix(c(16,6,4,14),nrow=2)
megfigyelt
chisq.test(megfigyelt,correct=FALSE)
chisq.test(megfigyelt)
chisq.test(megfigyelt)$observed
chisq.test(megfigyelt)$expected


# 5. Az „Örül-e, hogy statisztikai szoftvert használunk?” kérdésre adott válaszok függetlenek-e a nemtől?
adat=read.csv2("/home/grus/Desktop/Aktualis/Moni/HUN_Feladatok_Moni/Adatbazisok/orvkerd2016.csv")
megfigyelt = table(adat$orul,adat$nem)
megfigyelt
chisq.test(megfigyelt,correct=FALSE)
chisq.test(megfigyelt)
chisq.test(megfigyelt)$observed
chisq.test(megfigyelt)$expected
fisher.test(megfigyelt)


#6. Vizsgáljuk a biostatisztika nehézségének megítélése (nehez változó) és a nem
# közötti kapcsolatot! A férfiak és a nők egyforma arányban tartják nehéznek a biostatisztikát?
megfigyelt = table(adat$nehez,adat$nem)
megfigyelt
chisq.test(megfigyelt,correct=FALSE)
chisq.test(megfigyelt)
chisq.test(megfigyelt)$observed
chisq.test(megfigyelt)$expected
fisher.test(megfigyelt)



#7. Vizsgáljuk, hogy a ,,Mennyire szeret enni?'' (\variable{enni} változó) kérdésre adott válaszok függnek-e a nemtől!
megfigyelt = table(adat$eves,adat$nem)
megfigyelt
chisq.test(megfigyelt)
chisq.test(megfigyelt)$observed
chisq.test(megfigyelt)$expected
fisher.test(megfigyelt)
