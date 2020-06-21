# Librairies --------------------------------------------------------------

library("dplyr") # dataset manipulation
library("tidylog") # provide feedback about dplyr operations
library("mlr") # interface to classification and regression techniques
library("parallelMap") # parallelization framework
library("tidyr") # clean datasets
library("zoo") # missing values imputation

# Data --------------------------------------------------------------------

# Load data
train_data2017 <- read.csv("outputs/individus_2017_alldata.csv")
test_data2018 <- read.csv("outputs/individus_2018_alldata.csv")

# Création d'une fonction de cleaning et sélection de variables
cleaning <- function(dataframe){
  dataframe <- dataframe %>% 
    mutate(grav = ifelse(grav %in% c("Blessé hospitalisé", "Tué"), "Blessé grave", as.character(grav)),
           dep = as.factor(dep),
           place = as.factor(place),
           grav = factor(grav, levels = c("Indemne", "Blessé léger", "Blessé grave"))
  ) %>%
  rename(nb_pers_impl = nb_pers_impliquées)
  
  return(dataframe)
}

# Data cleaning sur les dataframe de test et train
train_data2017 <- cleaning(train_data2017)
test_data2018 <- cleaning(test_data2018)

# Gestion des valeurs manquantes ------------------------------------------

#' La gestion des valeurs manquantes est réalisée différemment selon les variables.
#' Sur certaines variables quali, nous ajoutons simplement un champs "Non déterminé".
#' Sur d'autres au contraire, nous souhaitons imputer la moyenne ou la modalité la plus représentée,
#' par groupe.

# Création d'une fonction qui renvoit la modalité la plus représentée
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Création d'une fonction de gestion des valeurs manquantes
gestion_na <- function(dataframe) {
  
  # Pour permettre l'ajout de modalités, remplacer les variables factor en char
  for (i in 1:ncol(dataframe)) {
    if (is.factor(dataframe[, i]) &
        i != which(colnames(dataframe) == "grav")) {
      dataframe[, i] <- as.character(dataframe[, i])
    }
  }
  
  # Imputation par la moyenne ou la modalité la plus représentée par groupe pour certaines variables
  dataframe <- dataframe  %>%
    group_by(catu, sexe) %>%
    mutate(age = na.aggregate(age, FUN = mean)) %>%
    ungroup() %>%
    group_by(cat_route, agg, catu) %>%
    mutate(collision = na.aggregate(collision, FUN = Mode)) %>%
    ungroup()
  
  # Pour les variables non traitées :
  # > pour les variables quali : ajout de la modalité "non communiqué"
  # > pour les variables quanti : imputation par la moyenne
  dataframe <- dataframe %>%
    mutate_if(is.character, ~ replace(., is.na(.), "Non communiqué")) %>%
    mutate_if(is.integer, ~ replace(., is.na(.), mean(., na.rm = TRUE)))
  
  # le package zoo change le format du dataframe
  dataframe <- data.frame(dataframe)
  
  # re transformation des données en facteurs
  for (i in 1:ncol(dataframe)) {
    if (is.character(dataframe[, i])) {
      dataframe[, i] <- as.factor(dataframe[, i])
    }
  }
  return(dataframe)
}

# Data cleansing et gestion des NA 
train_data2017 <- gestion_na(train_data2017)
test_data2018 <- gestion_na(test_data2018)

# Data check
sapply(train_data2017, function(x) sum(is.na(x)))
sapply(test_data2018, function(x) sum(is.na(x)))
summary(train_data2017)
summary(test_data2018)

# Random forest -----------------------------------------------------------

# Tasks 

trainTask = makeClassifTask(data = train_data2017, target = "grav")
testTask = makeClassifTask(data = test_data2018, target = "grav")
ln <- listLearners(trainTask)

# Learner 
getParamSet("classif.ranger")
rf.learner <- makeLearner("classif.ranger",
                          predict.type = "prob", 
                          importance = c("permutation")
                          )

# Train model
parallelStartSocket(5)

model <- train(rf.learner, 
                   trainTask)

parallelStop()

# Importance des variables
imp <- getFeatureImportance(model)
imp <- t(imp$res)

# Test model --------------------------------------------------------------

# Predictions
pred <- predict(model, newdata = test_data2018)

# Confusion matrix
calculateConfusionMatrix(pred, relative = FALSE, sums = FALSE, set = "both")

# Performances de la prédiction
performance(pred, measures = mmce) # Taux d'erreur : 0.3411442 // après sélection de var : 0.3476764
performance(pred, measures = acc) # Accuracy : 0.6588558 // après sélection de var : 0.6523236

# Note : il est possible de jouer sur le seuil ici :
# performance(setThreshold(r$pred, 0.5), measures = list(credit.costs, mmce))

#' Résultats
#' taux bien classés = 66%, taux mal classés = 34%

# Taux de bien classés par catégorie usager
bc <- data.frame(test_data2018 %>% select(catu, grav), 
                 pred$data[,c("truth", "response")])

bc %>% 
  group_by(catu) %>% 
  mutate(n_catu = n()) %>% 
  filter(truth == response) %>% 
  summarize(bien_classés = n(),
            n_catu = max(n_catu),
            taux_bien_classés = round(bien_classés/n_catu*100,1))

# Ce sont surtout les passagers pour lesquelson n'arrive pas à correctement prédire l'état de santé.

# Taux de bien classés par catégorie usager et gravité de l'accident
bc %>% 
  group_by(catu, grav) %>% 
  mutate(n_catu_grav = n()) %>% 
  filter(truth == response) %>% 
  summarize(bien_classés = n(),
            n_catu_grav = max(n_catu_grav),
            taux_bien_classés = round(bien_classés/n_catu_grav*100,1))

# Plot predictions (test)
parallelStartSocket(5)
plotLearnerPrediction(rf.learner, 
                      task = trainTask, 
                      features = c("age", "nbv"), 
                      cv = 0)

parallelStop()

# Utilisation d'une matrice de coûts --------------------------------------

#' L'accuracy n'est pas le principal KPI à prendre en compte.
#' L'objectif du modèle est de prévoir correctement le besoin de matériel sur le lieu d'intervention.
#' Ainsi, nous souhaitons principalement optimiser le bon classement des blessés hospitalisés qui ont besoin
#' de plus de matériel d'intervention. A l'inverse, nous n'accordons que peu d'importance à un blessé prédit 
#' léger alors qu'il est indemne, par exemple. 
#' Pour cela, nous décidons de mettre en place une matrice de coûts :

costs = matrix(c(0, 1, 4,  # true : indemne
                 3, 0, 3,  # true : Blessé léger
                 5, 3, 0),  # true : blessé grave
               nrow = 3, 
               byrow = T) 

colnames(costs) = rownames(costs) = getTaskClassLevels(trainTask)

# Création de la mesure du coût
rf.costs = makeCostMeasure(costs = costs,
                           best = 0, worst = 5)

# Calcul des seuils : 1/(average costs of true classes)
th = 1/rowSums(costs)
names(th) = getTaskClassLevels(trainTask)
th

pred.th = setThreshold(pred, threshold = th)

# Performance
performance(pred, measures = list(rf.costs, mmce)) #costs : 1.0259951 / mmce : 0.3411442 
performance(pred.th, measures = list(rf.costs, mmce)) # costs : 1.0418401 / mmce : 0.3539575
# Diminution des coûts, augmentation du taux d'erreur : il faut trouver un équilibre.

# Matrice de confusion
calculateConfusionMatrix(pred, relative = FALSE, sums = FALSE, set = "both")
calculateConfusionMatrix(pred.th, relative = FALSE, sums = FALSE, set = "both")


# Random forest - SPLIT ---------------------------------------------------

# Tasks
cond_train <- train_data2017 %>% 
  filter(catu == 'Conducteur') %>% 
  select(-catu, -place, -loc_pieton, 
         #-action_pieton
         )

trainTask_cond = makeClassifTask(data = cond_train, 
                                 target = "grav")

pass_train <- train_data2017 %>% 
  filter(catu == 'Passager') %>% 
  select(-catu, -loc_pieton, 
         #-action_pieton
         )

trainTask_pass = makeClassifTask(data = pass_train, 
                                 target = "grav")

piet_train <- train_data2017 %>% 
  filter(catu == 'Piéton') %>% 
  select(-catu, - place, - presence_PL)

trainTask_piet = makeClassifTask(data = piet_train, 
                                 target = "grav")

cond_test <- test_data2018 %>% 
  filter(catu == 'Conducteur') %>% 
  select(-catu, -place, -loc_pieton, 
         #-action_pieton
         )

testTask_cond = makeClassifTask(data = cond_test, 
                                target = "grav")

pass_test <- test_data2018 %>% 
  filter(catu == 'Passager') %>% 
  select(-catu, -loc_pieton, 
         #-action_pieton
         )

testTask_pass = makeClassifTask(data = pass_test, 
                                target = "grav")

piet_test <- test_data2018 %>% 
  filter(catu == 'Piéton') %>% 
  select(-catu, - place, - presence_PL)

testTask_piet = makeClassifTask(data = piet_test, 
                                target = "grav")


# Learner 
rf.learner <- makeLearner("classif.ranger",
                          predict.type = "prob", 
                          importance = c("permutation")
)

# Train model
parallelStartSocket(5)
model_cond <- train(rf.learner, trainTask_cond)
model_pass <- train(rf.learner, trainTask_pass)
model_piet <- train(rf.learner, trainTask_piet)
parallelStop()

# Importance des variables
imp_cond <- getFeatureImportance(model_cond)
imp_cond <- t(imp_cond$res)

imp_pass <- getFeatureImportance(model_pass)
imp_pass <- t(imp_pass$res)

imp_piet <- getFeatureImportance(model_piet)
imp_piet <- t(imp_piet$res)

# Test model

# Predictions
pred_cond <- predict(model_cond, 
                     newdata = test_data2018 %>% filter(catu == "Conducteur") %>% select(-catu, -place, -loc_pieton, 
                                                                                         #-action_pieton
                                                                                         ))
pred_pass <- predict(model_pass, 
                     newdata = test_data2018 %>% filter(catu == "Passager") %>% select(-catu, -loc_pieton, 
                                                                                       #-action_pieton
                                                                                       ))
pred_piet <- predict(model_piet, 
                     newdata = test_data2018 %>% filter(catu == "Piéton") %>% select(-catu, - place, - presence_PL))

# Confusion matrix
calculateConfusionMatrix(pred_cond, relative = FALSE, sums = FALSE, set = "both")
calculateConfusionMatrix(pred_pass, relative = FALSE, sums = FALSE, set = "both")
calculateConfusionMatrix(pred_piet, relative = FALSE, sums = FALSE, set = "both")

# Performances de la prédiction
performance(pred_cond, measures = acc) # Accuracy = 0.69
performance(pred_pass, measures = acc) # Accuracy = 0.53
performance(pred_piet, measures = acc) # Accuracy = 0.67

# Cost
performance(pred_cond, measures = list(rf.costs)) # Cost = 0.96
performance(pred_pass, measures = list(rf.costs)) # Cost = 1.29
performance(pred_piet, measures = list(rf.costs)) # Cost = 0.98 



# Save model --------------------------------------------------------------

saveRDS(model_cond, "rf_conducteurs.RDS")
saveRDS(model_pass, "rf_passagers.RDS")
saveRDS(model_piet, "rf_pietons.RDS")



str(train_data2017)



# test

test <- data.frame(test_data2018, 
                 pred$data[,c("truth", "response")])

test <- test %>% 
  filter(response == "Indemne")

write.csv(test, "test.csv")


