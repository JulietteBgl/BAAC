#------------------------------------------Libraries

install.packages("corrplot") # corrélation
install.packages("ggplot2") # data visualisation
install.packages("forcats") # factor manipulation
install.packages("dplyr") # dataset manipulation
install.packages("DataExplorer") # explorateur de données
install.packages("FactoMineR")# factorisation
install.packages("Factoshiny")# ACM Intérface API
install.packages("caret")
install.packages("VIM")
install.packages("doParallel")
install.packages("randomForest")



library("corrplot") # corrélation
library("ggplot2") # data visualisation
library("forcats") # factor manipulation
library("dplyr") # dataset manipulation
library("FactoMineR")# factorisation
library("Factoshiny") # ACM Intérface API
library("caret")
library("VIM")
library("doParallel")
library("randomForest")

# Préparation de la base globale


global_ind <- read.csv("outputs/individus_2017_alldata.csv",encoding = "UTF-8")

sapply(global_ind, function(x) sum(is.na(x)))
sapply(global_ind, function(x) sum(is.na(x))/nrow(global_ind)*100)

str(global_ind)

table(global_ind$catu, global_ind$utilisation_equipement_secu, useNA = "always")


global_ind <- global_ind %>%  mutate_if(is.factor,as.character)%>%
  mutate_if(is.character, ~replace(., is.na(.), "Non communiqué")) %>% 
  mutate_if(is.character,as.factor)%>%
  mutate_if(is.integer, ~replace(., is.na(.), mean(., na.rm = TRUE)))


table(global_ind$trajet)

data_mod <- global_ind %>% 
  select(-Num_Acc, -lat, -long, -groupe_age, -dep)

summary(data_mod)

table(global_ind$plan)

table(data_mod$grav)

# Transformer les variables Quali en variables Quanti 
library("caret")
dummy_variables <- dummyVars(~., data= data_mod)
data_mod_dicho <- predict(dummy_variables, newdata= data_mod) # Le jeu de données s'est transformé en matrix car il n'y a plus que du numérique
data_mod_dicho <- as.data.frame(data_mod_dicho) # Attention, la variable à expliquer est aussi dichotomisée
data_mod_dicho$"grav" <- ifelse(data_mod_dicho$"grav.Indemne" == 1, "Indemne", 
                                ifelse(data_mod_dicho$"grav.Tué" == 1, "Tué", 
                                       ifelse(data_mod_dicho$"grav.Blessé hospitalisé" == 1, "Blessé hospitalisé", 
                                              "Blessé léger"))) # Revenir à Grav initiale

table(data_mod_dicho$grav)

# On supprime les colonnes inutiles
data_mod_dicho <- data_mod_dicho %>% 
  select(-"grav.Indemne", -"grav.Tué", -"grav.Blessé hospitalisé", -"grav.Blessé léger")

data_mod_dicho$grav <- as.factor(data_mod_dicho$grav)

# Nombre d'individus complètement renseignés (Aucune valeur manquante)
nrow(na.omit(data_mod_dicho))

# Nombres de valeurs manquantes par variable
library("VIM")
summary(aggr(data_mod_dicho, plot=FALSE))


# Construire échantillon d'apprentissage et de Test
set.seed(123)
n <- nrow(data_mod_dicho)
perm<-sample(n) 
data_mod_dicho<-data_mod_dicho[perm[1:n],] 

# Tirage représentatif
library(caret)
tirage <- caret::createDataPartition(data_mod_dicho$grav, p = 0.5, list = FALSE)
global_ind_apprentissage <- data_mod_dicho[tirage,]
global_ind_test <- data_mod_dicho[-tirage,]

dim(global_ind_apprentissage)
dim(global_ind_test)

table(data_mod_dicho$grav)

prop.table(table(global_ind_apprentissage$grav))*100
prop.table(table(global_ind_test$grav))*100

# MODELISATION AVEC RANDOM FOREST
T1<-Sys.time()
library("doParallel")
cl <- makePSOCKcluster(4) 
registerDoParallel(cl)
# Fin Ouverture du cluster

global_ind_apprentissage <- global_ind_apprentissage %>% 
  select(catu.Conducteur, catu.Passager, catu.Piéton, sexe.Masculin, sexe.Féminin, grav)

fitControl <- trainControl(method="LGOCV")

rf1_model_500 <- caret::train(grav~., 
                              data=global_ind_apprentissage,
                              method = "ranger", 
                              #preProcess = c("scale", "center"),
                              #metric = "ROC",
                              trControl = fitControl,
                              #tuneGrid = rf_grid,
                              num.trees = 500)

rf1_model_500 <-randomForest(grav~.,data=global_ind_apprentissage,ntree=200,na.action=na.roughfix)
  
rf1_model_500
  
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