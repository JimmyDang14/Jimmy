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
matrixNHabitants_00_19prov <- matrixNHabitants_00_19[,-(21:22)]
matrixratioDIprov <- sweep(matrixDepensesInvestiss_00_17, 1, matrixNHabitants_00_19prov, "/")
matrixratioDI <- matrixratioDIprov * 1000
ratioDIprov <- as.data.frame(matrixratioDI)
ratioDIprov[1] <- NULL
ratioDIprov2 <- head(cbind(Commune, ratioDIprov), 46)
ratioDI <- ratioDIprov2[,-1]
rownames(ratioDI) <- ratioDIprov2[,1]
rm(matrixNHabitants_00_19prov,
   matrixDepensesInvestiss_00_17, matrixNHabitants_00_19,
   matrixratioDIprov, matrixratioDI, ratioDIprov, ratioDIprov2)
#3.2) Loyermoyen100m2 - Pas de ratio, seulement clean
Loyermoyen100m2_06_19[1] <- NULL
Loyermoyen100m2_06_19prov <- head(cbind(Commune, Loyermoyen100m2_06_19), 46)
Loyermoyen100m2_06_19_OK <- Loyermoyen100m2_06_19prov[,-1]
rownames(Loyermoyen100m2_06_19_OK) <- Loyermoyen100m2_06_19prov[,1]
rm(Loyermoyen100m2_06_19, Loyermoyen100m2_06_19prov)
#3.3) Ratio Ecole primaire (nombre d'écoles primaires moyen par km2)
matrixNEcolePrimaire <- data.matrix(NEcolePrimaire)
matrixSuperficie <- data.matrix(Superficie)
matrixratio_ecoleprov <- sweep(matrixNEcolePrimaire, 1, matrixSuperficie, "/")
ratio_ecoleprov <- as.data.frame(matrixratio_ecoleprov)
ratio_ecoleprov[1] <- NULL
ratio_ecoleprov2 <- head(cbind(Commune, ratio_ecoleprov), 46)
ratio_ecole <- ratio_ecoleprov2[,-1]
rownames(ratio_ecole) <- ratio_ecoleprov2[,1]
rm(matrixNEcolePrimaire, matrixSuperficie, matrixratio_ecoleprov, ratio_ecoleprov,ratio_ecoleprov2)
#3.4) NHabitants - Pas de ratio seulement clean
NHabitants_00_19[1] <- NULL
NHabitantsprov <- head(cbind(Commune, NHabitants_00_19), 46)
NHabitants_OK <- NHabitantsprov[,-1]
rownames(NHabitants_OK) <- NHabitantsprov[,1]
rm(NHabitants_00_19, NHabitantsprov)
#3.5) Ratio Infraction pour 1000 habitants
matrixNInfractions_08_19 <- data.matrix(NInfractions_08_19)
matrixNHabitants_00_19 <- data.matrix(NHabitants_00_19)
matrixNHabitants_00_19prov <- matrixNHabitants_00_19[,-(3:10)]
matrixratioInfractionprov <- sweep(matrixNInfractions_08_19, 1, matrixNHabitants_00_19prov, "/")
matrixratioInfraction <- matrixratioInfractionprov * 1000
ratioInfractionprov <- as.data.frame(matrixratioInfraction)
ratioInfractionprov[1] <- NULL
ratioInfractionprov2 <- head(cbind(Commune, ratioInfractionprov), 46)
ratioInfraction <- ratioInfractionprov2[,-1]
rownames(ratioInfraction) <- ratioInfractionprov2[,1]
rm(  matrixNInfractions_08_19, matrixNHabitants_00_19, matrixNHabitants_00_19prov,
   matrixratioInfractionprov,matrixratioInfraction,ratioInfractionprov,ratioInfractionprov2)
#3.6) Ratio Medecin pour 1000 habitants
matrixNMedecins <- data.matrix(NMedecins)
matrixNHabitants_00_19 <- data.matrix(NHabitants_00_19)
matrixNHabitants_00_19prov <- matrixNHabitants_00_19[,-(21:22)]
matrixNMedecinsprov <- sweep(matrixNMedecins, 1, matrixNHabitants_00_19prov, "/")
matrixratioMedecins <- matrixNMedecinsprov * 1000
ratioMedecinsprov <- as.data.frame(matrixratioMedecins)
ratioMedecinsprov[1] <- NULL
ratioMedecinsprov2 <- head(cbind(Commune, ratioMedecinsprov), 46)
ratioMedecins <- ratioMedecinsprov2[,-1]
rownames(ratioMedecins) <- ratioMedecinsprov2[,1]
rm(  matrixNMedecins, matrixNHabitants_00_19 ,matrixNHabitants_00_19prov,matrixNMedecinsprov,matrixratioMedecins,ratioMedecinsprov ,ratioMedecinsprov2 )

#3.7) Ratio Parcs par km2
matrixNParcs <- data.matrix(NParcs)
matrixSuperficie <- data.matrix(Superficie)
matrixratio_NParcsprov <- sweep(matrixNParcs, 1, matrixSuperficie, "/")
ratio_NParcsprov <- as.data.frame(matrixratio_NParcsprov)
ratio_NParcsprov[1] <- NULL
ratio_NParcsprov2 <- head(cbind(Commune, ratio_NParcsprov), 46)
ratio_NParcs <- ratio_NParcsprov2[,-1]
rownames(ratio_NParcs) <- ratio_NParcsprov2[,1]
rm(matrixNParcs, matrixSuperficie, matrixratio_NParcsprov, ratio_NParcsprov,ratio_NParcsprov2)

#3.8) Ratio Supermarches par km2
matrixNSupermarches <- data.matrix(NSupermarches)
matrixNSupermarchesprov <- matrixNSupermarches[,-(4:9)]
matrixSuperficie <- data.matrix(Superficie)
matrixratio_NSupermarchesprov <- sweep(matrixNSupermarchesprov, 1, matrixSuperficie, "/")
ratio_NSupermarchesprov <- as.data.frame(matrixratio_NSupermarchesprov)
ratio_NSupermarchesprov[1] <- NULL
ratio_NSupermarchesprov2 <- head(cbind(Commune, ratio_NSupermarchesprov), 46)
ratio_Supermarches <- ratio_NSupermarchesprov2[,-1]
rownames(ratio_Supermarches) <- ratio_NSupermarchesprov2[,1]
rm(matrixNSupermarches, matrixNSupermarchesprov,matrixSuperficie, matrixratio_NSupermarchesprov, ratio_NSupermarchesprov,ratio_NSupermarchesprov2)

#3.9) Prix médiane appartement 100m2 - Pas de ratio, seulement clean
PMedianeAppart100m2_00_17[1] <- NULL
PMedianeAppart100m2_00_17prov <- head(cbind(Commune, PMedianeAppart100m2_00_17), 46)
PMedianeAppart100m2_00_17_OK <- PMedianeAppart100m2_00_17prov[,-1]
rownames(PMedianeAppart100m2_00_17_OK) <- PMedianeAppart100m2_00_17prov[,1]
rm(PMedianeAppart100m2_00_17, PMedianeAppart100m2_00_17prov)

#3.10) Prix médiane Villa - Pas de ratio, seulement clean - Aucun détail sur dimensions du bien
PMedianeVilla_00_17[1] <- NULL
PMedianeVilla_00_17prov <- head(cbind(Commune, PMedianeVilla_00_17), 46)
PMedianeVilla_00_17_OK <- PMedianeVilla_00_17prov[,-1]
rownames(PMedianeVilla_00_17_OK) <- PMedianeVilla_00_17prov[,1]
rm(PMedianeVilla_00_17, PMedianeVilla_00_17prov)

#3.11) Densité de population
matrixNHabitants_00_19 <- data.matrix(NHabitants_00_19)
matrixSuperficie <- data.matrix(Superficie)
matrixSuperficieprov <- matrixSuperficie[,-(1:2)]
matrixDensite_population <- sweep(matrixNHabitants_00_19, 1, matrixSuperficieprov, "/")
Densite_populationprov <- as.data.frame(matrixDensite_population)
Densite_populationprov[1] <- NULL
Densite_populationprov2 <- head(cbind(Commune, Densite_populationprov), 46)
Densite_population <- Densite_populationprov2[,-1]
rownames(Densite_population) <- Densite_populationprov2[,1]
rm(matrixSuperficieprov,
   matrixSuperficie, matrixNHabitants_00_19,
   matrixDensite_population, Densite_populationprov, Densite_populationprov2)



###4.0) FONCTIONS PLOT------------------------------
#4.1) DI (funratioDIplot("nom de la commune")) & examples
fplotratioDI <- function(a){x <- ratioDI[c(a),c(2:19)]
tx <- t(x)
plot( tx,type="l", col="blue", lwd=2, xlab="Year", xaxt = "n", ylab="CHF",main="Dépense moyenne d'Investissement par habitant")
axis(1, at=1:18, labels=2000:2017)}
fplotratioDI("Genève")

#4.2) Loyermoyen100m2 (funloyermoyen100m2plot("nom de la commune"))
fplotloyer <- function(a){x <- Loyermoyen100m2_06_19_OK[c(a),c(2:15)]
tx <- t(x)
plot( tx,type="l", col="blue", lwd=2, xlab="Year", xaxt = "n", ylab="CHF", ylim = c(1000,2500), main="Loyer moyen appartement 100m2")
axis(1, at=1:14, labels=2006:2019)}
fplotloyer("Genève")

#4.3) Pas besoin pour école car ni la superficie ni le nombre d'école varie (malheureusement, pour cette dernière variable, j'ai pas les dates d'entrées)
#4.4) Population
fplotpopulation <- function(a){x <- NHabitants_OK[c(a),c(2:21)]
tx <- t(x)
plot( tx,type="l", col="blue", lwd=2, xlab="Year", xaxt = "n", ylab="Nombre d'habitants", main="Population")
axis(1, at=1:20, labels=2000:2019)}
fplotpopulation("Genève")
#4.5) Ratio Infraction pour 1000 habitants

fplotinfraction <- function(a){x <- ratioInfraction[c(a),c(2:13)]
tx <- t(x)
plot( tx,type="l", col="blue", lwd=2, xlab="Year", xaxt = "n", ylab="Nombre d'infractions",main="Nombre d'infractions du Code Pénal par 1000 habitants")
axis(1, at=1:12, labels=2008:2019)}
fplotinfraction("Genève")

#4.6) Pas besoin pour médecin parce que j'ai pas les dates d'entrée en service des médecins, du coup un plot serait questionnable pour voir l'évolution

#4.7) Pas besoin pour les parcs, comme pour école

#4.8) Pas besoin pour les supermarchés comme pour école

#4.9) Prix médiane appartement 100m2 
fplotprixappart <- function(a){x <- PMedianeAppart100m2_00_17_OK[c(a),c(2:19)]
tx <- t(x)
plot( tx,type="l", col="blue", lwd=2, xlab="Year", xaxt = "n", ylab="CHF", main="Prix médiane d'un appartement 100m2")
axis(1, at=1:18, labels=2000:2017)}
fplotprixappart("Genève")

#4.10) Prix médiane Villa
fplotprixvilla <- function(a){x <- PMedianeVilla_00_17_OK[c(a),c(2:19)]
tx <- t(x)
plot( tx,type="l", col="blue", lwd=2, xlab="Year", xaxt = "n", ylab="CHF", main="Prix médiane d'une villa (taille inconnue)")
axis(1, at=1:18, labels=2000:2017)}
fplotprixvilla("Genève")

#4.11) Densite population (plot discutable vu que c'est la même courbe que celle de la population)
fplotdensitepopulation <- function(a){x <- Densite_population[c(a),c(2:21)]
tx <- t(x)
plot( tx,type="l", col="blue", lwd=2, xlab="Year", xaxt = "n", ylab="Nombre de personnes/km2",main="Densité de la population")
axis(1, at=1:20, labels=2000:2019)}
fplotdensitepopulation("Genève")

##5) FONCTIONS POUR OBTENIR DONNEES VOULUES D'UNE COMMUNE A UNE CERTAINE ANNEE------------------------------------------------------
#5.1) DI
freturnDI <- function(a,b){x <- ratioDI[c(a),c(b)]
return(x)
}
freturnDI("Genève","2010")

#5.2) Loyermoyen100m2 
freturnloyer <- function(a,b){x <- Loyermoyen100m2_06_19_OK[c(a),c(b)]
return(x)
}
freturnloyer("Genève","2010")

#5.3) Ratio Ecole
freturnecole <- function(a){x <- ratio_ecole[c(a), c(2)]
return(x)
}
freturnecole("Genève")

#5.4) Population
freturnpopulation <- function(a,b){x <- NHabitants_OK[c(a),c(b)]
return(x)
}
freturnpopulation("Genève","2010")

#5.5) Ratio Infraction pour 1000 habitants
freturninfraction <- function(a,b){x <- ratioInfraction[c(a),c(b)]
return(x)
}
freturninfraction("Genève","2010")

#5.6) Ratio Medecin (pour 1000 habitants)
freturnmedecin <- function(a){x <- ratioMedecins[c(a), c(2)]
return(x)
}
freturnmedecin("Genève")

#5.7) Ratio Parc
freturnparc <- function(a){x <- ratio_NParcs[c(a), c(2)]
return(x)
}
freturnparc("Genève")

#5.8) Ratio Supermarche
freturnsupermarche <- function(a){x <- ratio_Supermarches[c(a), c(2)]
return(x)
}
freturnsupermarche("Genève")

#5.9) Prix médiane appartement 100m2 
freturnprixappart <- function(a,b){x <- PMedianeAppart100m2_00_17_OK[c(a),c(b)]
return(x)
}
freturnprixappart("Genève","2010")

#5.10) Prix médiane Villa
freturnprixvilla <- function(a,b){x <- PMedianeVilla_00_17_OK[c(a),c(b)]
return(x)
}
freturnprixvilla("Genève","2010")

#5.11) Densite population 
freturndensitepopulation <- function(a,b){x <- Densite_population[c(a),c(b)]
return(x)
}
freturndensitepopulation("Genève","2010")
#5.12) Superficie km2
freturnsuperficie <- function(a){x <- ratio_ecole[c(a), c(2)]
return(x)
}
freturnsuperficie("Genève")


##6) CLEAN USELESS DATA------------------------------------------------------
rm(Commune, DepensesInvestiss_00_17,NEcolePrimaire, NInfractions_08_19, NMedecins, NParcs, NSupermarches)


##7) LISTE FONCTIONS-------------------------------------------
fplotratioDI("Genève")

fplotloyer("Genève")

fplotpopulation("Genève")

fplotinfraction("Genève")

fplotprixappart("Genève")

fplotprixvilla("Genève")

fplotdensitepopulation("Genève")



freturnDI("Genève","2010")

freturnloyer("Genève","2010")

freturnecole("Genève")

freturnpopulation("Genève","2010")

freturninfraction("Genève","2010")

freturnmedecin("Genève")

freturnparc("Genève")

freturnsupermarche("Genève")

freturnprixappart("Genève","2010")

freturnprixvilla("Genève","2010")

freturndensitepopulation("Genève","2010")

freturnsuperficie("Genève")

