rm(list = ls())
install.packages("devtools")
devtools::install_github("JimmyDang14/givedata")

###1.0)GET DATASETS-------------------------------------------------------
library(devtools)
library(dplyr)

library(givedata)
data("DepensesInvestiss_00_17", package="givedata")
ls()
DepensesInvestiss_00_17 %>% head()

data("Loyermoyen100m2_06_19", package="givedata")
ls()
Loyermoyen100m2_06_19 %>% head()

data("NEcolePrimaire", package="givedata")
ls()
NEcolePrimaire %>% head()

data("NHabitants_00_19", package="givedata")
ls()
NHabitants_00_19 %>% head()

data("NInfractions_08_19", package="givedata")
ls()
NInfractions_08_19 %>% head()

data("NMedecins", package="givedata")
ls()
NMedecins %>% head()

data("NParcs", package="givedata")
ls()
NParcs %>% head()

data("NSupermarches", package="givedata")
ls()
NSupermarches %>% head()

data("PMedianeAppart100m2_00_17", package="givedata")
ls()
PMedianeAppart100m2_00_17 %>% head()

data("PMedianeVilla_00_17", package="givedata")
ls()
PMedianeVilla_00_17 %>% head()

data("Superficie", package="givedata")
ls()
Superficie %>% head()

###2.0)DIVERS---------------------------------------
#Vector of Commune names
x <- c("NA","Aire-la-Ville","Anières", "Avully", "Avusy",
       " Bardonnex","Bellevue","Bernex","Carouge", "Cartigny",
       "Céligny", "Chancy",  "Chêne-Bougeries", "Chêne-Bourg",
       "Choulex","Collex-Bossy","Collonge-Bellerive","Cologny",
       "Confignon","Corsier","Dardagny","Genève","Genthod",
       "Grand-Saconnex","Gy","Hermance","Jussy","Laconnex",
       "Lancy","Meinier", "Meyrin", "Onex","Perly-Certoux",
       "Plan-les-Ouates", "Pregny-Chambésy", "Presinge", "Puplinge",
       "Russin","Satigny","Soral","Thônex","Troinex","Vandoeuvres",
       "Vernier","Versoix","Veyrier")
Commune <- matrix(x)
rm(x)

###3.0)CLEANDATA&RATIOS---------------------------------------
#3.1)Ratio Depenses d'Investissement de la Commune par habitant

matrixDepensesInvestiss_00_17 <- data.matrix(DepensesInvestiss_00_17)
matrixNHabitants_00_19 <- data.matrix(NHabitants_00_19)
matrixratioDIprov <- sweep(matrixDepensesInvestiss_00_17, 1, matrixNHabitants_00_19, "/")
matrixratioDI <- matrixratioDIprov * 1000
ratioDIprov <- as.data.frame(matrixratioDI)
ratioDIprov[1] <- NULL
ratioDIprov2 <- head(cbind(Commune, ratioDIprov), 46)
ratioDI <- ratioDIprov2[,-1]
rownames(ratioDI) <- ratioDIprov2[,1]
rm(matrixDepensesInvestiss_00_17, matrixNHabitants_00_19,
   matrixratioDIprov, matrixratioDI, ratioDIprov, ratioDIprov2)
#3.2) Loyermoyen100m2 - Pas de ratio, seulement clean
Loyermoyen100m2_06_19[1] <- NULL
Loyermoyen100m2_06_19prov <- head(cbind(Commune, Loyermoyen100m2_06_19), 46)
Loyermoyen100m2_06_19_OK <- Loyermoyen100m2_06_19prov[,-1]
rownames(Loyermoyen100m2_06_19_OK) <- Loyermoyen100m2_06_19prov[,1]
rm(Loyermoyen100m2_06_19, Loyermoyen100m2_06_19prov)
#3.3) Densité Ecole primaire (nombre d'écoles primaires moyen par km2)
matrixNEcolePrimaire <- data.matrix(NEcolePrimaire)
matrixSuperficie <- data.matrix(Superficie)
matrixdensite_ecoleprov <- sweep(matrixNEcolePrimaire, 1, matrixSuperficie, "/")
densite_ecoleprov <- as.data.frame(matrixdensite_ecoleprov)
densite_ecoleprov[1] <- NULL
densite_ecoleprov2 <- head(cbind(Commune, densite_ecoleprov), 46)
densite_ecole <- densite_ecoleprov2[,-1]
rownames(densite_ecole) <- densite_ecoleprov2[,1]
rm(matrixNEcolePrimaire, matrixSuperficie, matrixdensite_ecoleprov, densite_ecoleprov,densite_ecoleprov2)
#3.4) NHabitants - Pas de ratio seulement clean
NHabitants_00_19[1] <- NULL
NHabitantsprov <- head(cbind(Commune, NHabitants_00_19), 46)
NHabitants_OK <- NHabitantsprov[,-1]
rownames(NHabitants_OK) <- NHabitantsprov[,1]
rm(NHabitants_00_19, NHabitantsprov)
#3.5) Ratio Infraction pour 1000 habitants
NInfractions_08_19prov <- NInfractions_08_19[-1,]
colnames(NInfractions_08_19prov) <- NInfractions_08_19[1,]
matrixNInfractions_08_19 <- data.matrix(NInfractions_08_19prov)
matrixNHabitants_00_19 <- data.matrix(NHabitants_00_19)
matrixNHabitants_00_19prov <- matrixNHabitants_00_19[,-(3:10)]
matrixratioInfractionprov <- sweep(matrixNInfractions_08_19, 1, matrixNHabitants_00_19prov, "/")
matrixratioInfraction <- matrixratioInfractionprov * 1000
ratioInfractionprov <- as.data.frame(matrixratioInfraction)
ratioInfractionprov[1] <- NULL
ratioInfractionprov2 <- head(cbind(Commune, ratioInfractionprov), 46)
ratioInfraction <- ratioInfractionprov2[,-1]
rownames(ratioInfraction) <- ratioInfractionprov2[,1]
rm(NInfractions_08_19prov,
   matrixNInfractions_08_19, matrixNHabitants_00_19, matrixNHabitants_00_19prov,
   matrixratioInfractionprov,matrixratioInfraction,ratioInfractionprov,ratioInfractionprov2)

###4.0) FONCTIONS PLOT------------------------------
#4.1) DI (funratioDIplot("nom de la commune")) & examples

funratioDIplot <- function(a){x <- ratioDI[c(a),c(2:19)]
tx <- t(x)
plot( tx,type="l", col="blue", lwd=2, xlab="Year", xaxt = "n", ylab="CHF",main="Dépense moyenne d'Investissement par habitant")
axis(1, at=1:18, labels=2000:2017)}

funratioDIplot("Aire-la-Ville")
funratioDIplot("Choulex")
funratioDIplot("Genève")
#4.2) Loyermoyen100m2 (funloyermoyen100m2("nom de la commune"))
funloyermoyen100m2 <- function(a){x <- Loyermoyen100m2_06_19_OK[c(a),c(2:15)]
tx <- t(x)
plot( tx,type="l", col="blue", lwd=2, xlab="Year", xaxt = "n", ylab="CHF", ylim = c(1000,2500), main="Loyer moyen appartement 100m2")
axis(1, at=1:14, labels=2006:2019)}

funloyermoyen100m2("Genève")
#4.3) Pas besoin pour école car pas de variation sur le temps ( pas d'info du moins)
#4.4) NHabitants
funNHabitants <- function(a){x <- NHabitants_OK[c(a),c(2:15)]
tx <- t(x)
plot( tx,type="l", col="blue", lwd=2, xlab="Year", xaxt = "n", ylab="Nombre d'habitants", main="Population")
axis(1, at=1:20, labels=2000:2019)}

funNHabitants("Genève")
