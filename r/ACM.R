#------------------------------------------Libraries

install.packages("corrplot") # corrélation
install.packages("ggplot2") # data visualisation
install.packages("forcats") # factor manipulation
install.packages("dplyr") # dataset manipulation
install.packages("DataExplorer") # explorateur de données
install.packages("FactoMineR")# factorisation
install.packages("Factoshiny")# ACM Intérface API

library("corrplot") # corrélation
library("ggplot2") # data visualisation
library("forcats") # factor manipulation
library("dplyr") # dataset manipulation
library("FactoMineR")# factorisation
library("Factoshiny") # ACM Intérface API


#------------------------------------------Import données finales

individus_2017_alldata <- read.csv(file ="data/individus_2017_alldata.csv", header = TRUE, sep = ",",encoding="UTF-8")

#------------------------------------------Selection des données pour ACM


global_acc_f<-select(individus_2017_alldata,Num_Acc,grav,sexe,groupe_age,catu,trajet,utilisation_equipement_secu,choc,prof,plan,surf,lum,agg,int,zone,cat_vehic,meteo,collision,cat_route)

#------------------------------------------ACM

#Gravité en illustrative
#SD (sexe/age) en active
#Hors NA

res.mca<-MCA(global_acc_f[,2:ncol(global_acc_f)],quali.sup=1,graph=T,na.method="average")


#------------------------------------------Représentation de l'inertie

eig.val <- res.mca$eig
barplot(eig.val[, 2], 
        names.arg = 1:nrow(eig.val), 
        main = "Variances Explained by Dimensions (%)",
        xlab = "Principal Dimensions",
        ylab = "Percentage of variances",
        col ="steelblue")
lines(x = 1:nrow(eig.val), eig.val[, 2], 
      type = "b", pch = 19, col = "red")


#------------------------------------------Représentation individus/modalités


#modalités+individus
x11();plot(res.mca)

#variables
plot(res.mca,choix = "var",axes = 1:2)

#modalités
plot(res.mca, invisible = c("ind","quali.sup"),autoLab = "no",cex=0.7)
plot(res.mca, invisible = c("ind"),autoLab = "yes",cex=0.8,selectMod = "contrib 20")
plot(res.mca, invisible = c("ind"),autoLab = "yes",cex=0.8,axes = c(1,3) , selectMod = "contrib 30")

#individus
plot(res.mca, invisible = c("var","quali.sup"),habillage="grav")

#------------------------------------------Tableau d'inertie

summary.MCA(res.mca,ncp=5,nbelements = Inf)

res.mca$var$contrib

dimdesc(res.mca)


#------------------------------------------Tableau d'inertieRécupération des coordonnées

coord<-as.data.frame(res.mca$ind$coord[,1:11])
global_acc_f<-cbind(global_acc_f,coord)




#------------------------------------------Poubelle


#CrossTable(global_acc$grav, global_acc$groupe_age)

# table(global_acc$grav)
# 
# table(lieux$int)



#Test Factoshiny

# library(Factoshiny)
# 
# ourtmca<-MCAshiny(global_acc_f[,2:ncol(global_acc_f)])

# 
# 
# table(global_acc$atm_rec)
# table(global_acc$catv)
# table(vehic2017$catv)
# table(global_acc$catv_rec)
# table(global_acc$surf_rec)
# table(global_acc_f$utilisation_equipement_secu)

# # dplyr
# caract2017 %>% summarise_all(funs(n_distinct))
# # en combinant les deux :
# sapply(caract2017, n_distinct)
