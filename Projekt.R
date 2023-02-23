# Daten einlesen und kontrollieren
daten = read.table("daten.csv", header = TRUE, sep=",")
head(daten)
any(is.na(daten))
summary(daten)

# Datentypen anpassen
daten$extraversion_introversion<-as.factor(daten$extraversion_introversion)
daten$intuition_sensorik<-as.factor(daten$intuition_sensorik)
daten$denken_fuehlen<-as.factor(daten$denken_fuehlen)
daten$wahrnehmen_beurteilen<-as.factor(daten$wahrnehmen_beurteilen)
summary(daten)

# Nicht benötigte Daten entfernen und Datum formatieren
daten<-daten[,-c(7,8)]
summary(daten)
daten$geburtstag<-as.Date(daten$geburtstag, format='%d.%m.')
summary(daten)

# Sternzeichen berechnen
# install.packages("DescTools")
library(DescTools)
daten$sternzeichen<-Zodiac(daten$geburtstag, lang="deu")
summary(daten)
sternzeichen<-daten$sternzeichen

# Deskriptive Analyse der Stichprobe
barplot(table(sternzeichen), las=2)
abline(h=mean(length(sternzeichen)/12), col="red")
c1<-daten$extraversion_introversion
c2<-daten$intuition_sensorik
c3<-daten$denken_fuehlen
c4<-daten$wahrnehmen_beurteilen
par(mfrow=c(2,2))
barplot(table(c1))
abline(h=mean(length(c1)/2), col="red")
barplot(table(c2))
abline(h=mean(length(c2)/2), col="red")
barplot(table(c3))
abline(h=mean(length(c3)/2), col="red")
barplot(table(c4))
abline(h=mean(length(c4)/2), col="red")

# Kombinieren der einzelnen Kontingenztabellen zu einer gemeinsamen Tabelle
t1 = with(daten, table(sternzeichen, extraversion_introversion))
t2 = with(daten, table(sternzeichen, intuition_sensorik))
t3 = with(daten, table(sternzeichen, denken_fuehlen))
t4 = with(daten, table(sternzeichen, wahrnehmen_beurteilen))

tab_sternzeichen_charaktereigenschaft = cbind(t1, t2, t3, t4)
library(pander)
pander(tab_sternzeichen_charaktereigenschaft, justify="right", emphasize.rownames=FALSE)

# Die relativen Anteile werden in Zeilenprozent, also normiert auf die Sternzeichen, angegeben
tab_sternz_rel = round(prop.table(tab_sternzeichen_charaktereigenschaft, 1), 2)
pander(tab_sternz_rel, justify="right", emphasize.rownames=FALSE)

# Die relativen Häufigkeiten werden mithilfe eines Spineplots visualisiert
spineplot(tab_sternz_rel, main="Charaktereigenschaft nach Sternzeichen")

# Chi Quadrat Test wird durchgeführt
chisq.test(tab_sternzeichen_charaktereigenschaft)
chisq.test(tab_sternzeichen_charaktereigenschaft, simulate.p.value=TRUE, B=10000)