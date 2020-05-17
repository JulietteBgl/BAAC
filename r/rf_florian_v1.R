# Préparation de la base globale
library(tidyverse)

# Remplacement des NA
sapply(global_ind, function(x) sum(is.na(x))) # Nombre de NA par variable
sapply(global_ind, function(x) sum(is.na(x))/nrow(global_ind)*100) #Proportion de NA par variable

    # Remplacement des NA Pour la variable Trajet : On créé une modalité Inconnu
global_ind$trajet[is.na(global_ind$trajet)] <- "Inconnu" # On créé une modalité Inconnu

  # Remplacement des NA Pour la variable utilisation_equipement_secu : SI NA sur Passager et Conducteur, on passe à Non déterminable
table(global_ind$catu, global_ind$utilisation_equipement_secu, useNA = "always")

global_ind <- global_ind %>%
      mutate(
        utilisation_equipement_secu = ifelse(catu %in% c("Conducteur","Passager") & is.na(utilisation_equipement_secu), 
                    yes = "Non de´terminable", 
                    no = as.character(utilisation_equipement_secu))
            )
table(global_ind$catu, global_ind$utilisation_equipement_secu, useNA = "always")

  # Remplacement des NA Pour la variable utilisation_equipement_secu : Il reste les pietons : on supprimera la donnée après dichotomisation (car idem Pieton)
global_ind$utilisation_equipement_secu[is.na(global_ind$utilisation_equipement_secu)] <- "SecuPietonASupprimer" # On créé une modalité Inconnu

table(global_ind$catu, global_ind$utilisation_equipement_secu, useNA = "always")

  # Remplacement des NA Pour la variable choc : On créé une modalité Inconnu
global_ind$choc[is.na(global_ind$choc)] <- "Inconnu" # On créé une modalité Inconnu

  # Remplacement des NA Pour la variable Plan : On créé une modalité Inconnu
global_ind$plan[is.na(global_ind$plan)] <- "Inconnu" # On créé une modalité Inconnu

  # Remplacement des NA pour la moyenne (Variable quanti) ou Mode (Variable Quali) par Groupe
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

library(zoo)

global_ind <- global_ind  %>%
  group_by(catu, sexe) %>%
    mutate(
          age = na.aggregate(age, FUN=mean)
          ) %>%
  ungroup() %>%
  
  group_by(cat_route, agg) %>%
  mutate(
    nbv = na.aggregate(nbv, FUN=mean)
  ) %>%
  ungroup() %>%
  
  group_by(cat_route, agg) %>%
    mutate(
          prof = na.aggregate(prof, FUN=Mode),
          int = na.aggregate(int, FUN=Mode)
           ) %>%
  ungroup() %>%

  group_by(cat_route, agg, catu) %>%
  mutate(
    collision = na.aggregate(collision, FUN=Mode)
  ) %>%
  ungroup() %>%

  group_by(surf) %>%
  mutate(
    meteo = na.aggregate(meteo, FUN=Mode)
  ) %>%
  ungroup() %>%

  group_by(meteo) %>%
  mutate(
    surf = na.aggregate(surf, FUN=Mode)
  ) %>%
  ungroup() 

# FIN du remplacement des NA
str(global_ind)

dataflo <- global_ind %>% 
        select(-Num_Acc, -lat, -long, -groupe_age, -dep)

summary(dataflo)
table(dataflo$grav)

# Transformer les variables Quali en variables Quanti 
library("caret")
dummy_variables <- dummyVars(~., data= dataflo)
dataflo_dichoto <- predict(dummy_variables, newdata= dataflo) # Le jeu de données s'est transformé en matrix car il n'y a plus que du numérique
dataflo_dichoto <- as.data.frame(dataflo_dichoto) # Attention, la variable à expliquer est aussi dichotomisée
dataflo_dichoto$"grav" <- ifelse(dataflo_dichoto$"grav.Indemne" == 1, "Indemne", 
                          ifelse(dataflo_dichoto$"grav.Tué" == 1, "Tué", 
                          ifelse(dataflo_dichoto$"grav.Blessé hospitalisé" == 1, "Blessé hospitalisé", 
                          "Blessé léger"))) # Revenir à Grav initiale
table(dataflo_dichoto$grav)

# On supprime les colonnes inutiles
str(dataflo_dichoto)
dataflo_dichoto <- dataflo_dichoto %>% 
      select(-"grav.Indemne", -"grav.Tué", -"grav.Blessé hospitalisé", -"grav.Blessé léger", -"utilisation_equipement_secuSecuPietonASupprimer")

dataflo_dichoto$grav <- as.factor(dataflo_dichoto$grav)

# Nombre d'individus complètement renseignés (Aucune valeur manquante)
nrow(na.omit(dataflo_dichoto))

# Nombres de valeurs manquantes par variable
library("VIM")
summary(aggr(dataflo_dichoto, plot=FALSE))

# Construire échantillon d'apprentissage et de Test
set.seed(123)
n <- nrow(dataflo_dichoto)
perm<-sample(n) 
dataflo_dichoto<-dataflo_dichoto[perm[1:n],] 

# Tirage représentatif
library(caret)
tirage <- caret::createDataPartition(dataflo_dichoto$grav, p = 0.5, list = FALSE)
global_ind_apprentissage <- dataflo_dichoto[tirage,]
global_ind_test <- dataflo_dichoto[-tirage,]

dim(global_ind_apprentissage)
dim(global_ind_test)

prop.table(table(global_ind_apprentissage$grav))*100
prop.table(table(global_ind_test$grav))*100

# MODELISATION AVEC RANDOM FOREST
T1<-Sys.time()
library("doParallel")
cl <- makePSOCKcluster(4) 
registerDoParallel(cl)
# Fin Ouverture du cluster

global_ind_apprentissage <- global_ind_apprentissage %>% 
    select(catuConducteur, catuPassager, catuPiéton, sexe.Masculin, sexe.Féminin, grav, 
           lumJour, "lumNuit avec éclairage",
           "lumNuit avec éclairage",
           "lumNuit sans éclairage",
           "agg.Hors Agglo",
           "agg.Agglo", 
            "nb_pers_impliquées", "zoneIDF", "zoneProvince", 
            "cat_vehic2 roues > 125 cm3")

fitControl <- trainControl(method="LGOCV")

rf1_model_500 <- caret::train(grav~., 
                              data=global_ind_apprentissage,
                              method = "ranger", 
                              #preProcess = c("scale", "center"),
                              #metric = "ROC",
                              trControl = fitControl,
                              #tuneGrid = rf_grid,
                              num.trees = 500)
# Fermeture du cluster
on.exit(stopCluster(cl))
T2<-Sys.time()
# Fin Fermeture du cluster

# Temps de calcul
Tdiff <- difftime(T2, T1)
print(Tdiff)

print(rf1_model_500)
pred <- predict(rf1_model_500,newdata=global_ind_test)
print(table(pred))
table(global_ind_test$grav)
mat <- confusionMatrix(data=pred,reference=global_ind_test$grav)
print(mat)


# Brouillon
56%

rf_grid <- expand.grid(mtry = c(1, 2, 3, 4, 5, 6, 7),
                       splitrule = c("gini", "extratrees"),
                       min.node.size = c(1, 2, 5, 10, 20, 25, 30, 50))
rf_grid



fitControl <- caret::trainControl(classProbs=TRUE, 
                                  summary=twoClassSummary,
                                  sampling = "down",
                                  method = "repeatedcv",
                                  number = 8,
                                  repeats=2)

Prediction           Blessé hospitalisé Blessé léger Indemne   Tué
Blessé hospitalisé               4637         2915     289   731
Blessé léger                     2896         7204    2192   323
Indemne                          5485        10375   22185   615
Tué                                 6           10       4    18