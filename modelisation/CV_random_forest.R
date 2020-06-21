# Librairies --------------------------------------------------------------

library("dplyr") # dataset manipulation
library("tidylog") # provide feedback about dplyr operations
library("mlr") # interface to classification and regression techniques
library("parallelMap") # parallelization framework
library("tidyr") # clean datasets
library("zoo") # missing values imputation
detach("package:MASS", unload=TRUE)

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

# clean env
rm(cleaning, gestion_na, Mode)

# Mise en place -----------------------------------------------------------

# Learner 
getParamSet("classif.ranger")
rf.learner <- makeLearner("classif.ranger",
                          predict.type = "prob", 
                          importance = c("permutation")
                          )

# Random forest - toutes les données --------------------------------------

# Task 
trainTask = makeClassifTask(data = train_data2017, target = "grav")
ln <- listLearners(trainTask)

# cross validation - resample instance
# r_ins = makeResampleInstance("Holdout", split = 2/3, task = trainTask)
r_ins = makeResampleInstance("CV", iters = 5, task = trainTask)

# Utilisation d'une matrice de coûts

#' L'accuracy n'est pas le principal KPI à prendre en compte.
#' L'objectif du modèle est de prévoir correctement le besoin de matériel sur le lieu d'intervention.
#' Ainsi, nous souhaitons principalement optimiser le bon classement des blessés graves qui ont besoin
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

# Train model
parallelStartSocket(5)

r = resample(learner = rf.learner, 
             task = trainTask, 
             resampling = r_ins, 
             measures = list(rf.costs, mmce), 
             show.info = FALSE)

parallelStop()
r

# Resample Result
# Task: train_data2017
# Learner: classif.ranger
# Aggr perf: costs.test.mean=1.0401762,mmce.test.mean=0.3316706
# Runtime: 650.429

# Matrice de confusion
calculateConfusionMatrix(r$pred, relative = FALSE, sums = FALSE, set = "both")

# Taux de bien classés par catégorie d'usagers
bc <- r$pred$data[,c("id", "truth", "response")]
true <- train_data2017 %>% 
  mutate(id = seq(1:nrow(train_data2017))) %>% 
  select(id, catu)
bc <- bc %>% 
  left_join(true)

bc %>% 
  group_by(catu) %>% 
  mutate(n_catu = n()) %>% 
  filter(truth == response) %>% 
  summarize(bien_classés = n(),
            n_catu = max(n_catu),
            taux_mal_classés = 100 - round(bien_classés/n_catu*100,1))

# Random forest - conducteurs ---------------------------------------------

# Task
cond_train <- train_data2017 %>% 
  filter(catu == 'Conducteur') %>% 
  select(-catu, -place, -loc_pieton)

trainTask_cond = makeClassifTask(data = cond_train, 
                                 target = "grav")

# cross validation - resample instance
r_ins = makeResampleInstance("CV", iters = 5, task = trainTask_cond)

# Train model
parallelStartSocket(5)

r = resample(learner = rf.learner, 
             task = trainTask_cond, 
             resampling = r_ins, 
             measures = list(rf.costs, mmce), 
             show.info = FALSE)

parallelStop()
r

# Resample Result
# Task: cond_train
# Learner: classif.ranger
# Aggr perf: costs.test.mean=1.0057687,mmce.test.mean=0.3120786
# Runtime: 144.379

# Matrice de confusion
calculateConfusionMatrix(r$pred, relative = FALSE, sums = FALSE, set = "both")


# Random forest - passagers -----------------------------------------------

pass_train <- train_data2017 %>% 
  filter(catu == 'Passager') %>% 
  select(-catu, -loc_pieton)

trainTask_pass = makeClassifTask(data = pass_train, 
                                 target = "grav")

# cross validation - resample instance
r_ins = makeResampleInstance("CV", iters = 5, task = trainTask_pass)

# Train model
parallelStartSocket(5)

r = resample(learner = rf.learner, 
             task = trainTask_pass, 
             resampling = r_ins, 
             measures = list(rf.costs, mmce), 
             show.info = FALSE)

parallelStop()
r

# Resample Result
# Task: pass_train
# Learner: classif.ranger
# Aggr perf: costs.test.mean=1.2558596,mmce.test.mean=0.4308209
# Runtime: 24.7856

# Matrice de confusion
calculateConfusionMatrix(r$pred, relative = FALSE, sums = FALSE, set = "both")

# Random forest - piétons -------------------------------------------------

piet_train <- train_data2017 %>% 
  filter(catu == 'Piéton') %>% 
  select(-catu, -place, -presence_PL)

trainTask_piet = makeClassifTask(data = piet_train, 
                                 target = "grav")

# cross validation - resample instance
r_ins = makeResampleInstance("CV", iters = 5, task = trainTask_piet)

# Train model
parallelStartSocket(5)

r = resample(learner = rf.learner, 
             task = trainTask_piet, 
             resampling = r_ins, 
             measures = list(rf.costs, mmce), 
             show.info = FALSE)

parallelStop()
r

# Resample Result
# Task: piet_train
# Learner: classif.ranger
# Aggr perf: costs.test.mean=0.9550632,mmce.test.mean=0.3225654
# Runtime: 7.4717

# Matrice de confusion
calculateConfusionMatrix(r$pred, relative = FALSE, sums = FALSE, set = "both")

# Erreur de généralisation ------------------------------------------------

# Après avoir choisi les modèles à utiliser en cross validation de type k-fold, on peut tester
# le modèle sélectionné sur des données qui n'ont pas encore été vues : les données de 2018.

# Etape 1 : entrainer le modèle sélectionné sur les données 2017
rf.learner <- makeLearner("classif.ranger",
                          predict.type = "prob", 
                          importance = c("permutation"))

trainTask = makeClassifTask(data = train_data2017, target = "grav")

parallelStartSocket(5)
modele <- train(rf.learner, trainTask)
parallelStop()

# Etape 2 : utiliser le modèle sur les données 2018
pred <- predict(modele, 
                newdata = test_data2018)

# Etape 3 : Evaluer le modèle

# Utilisation d'une matrice de coûts
costs = matrix(c(0, 1, 4,  # true : indemne
                 3, 0, 3,  # true : Blessé léger
                 5, 3, 0),  # true : blessé grave
               nrow = 3, 
               byrow = T) 

colnames(costs) = rownames(costs) = getTaskClassLevels(trainTask)

rf.costs = makeCostMeasure(costs = costs,
                           best = 0, worst = 5)

# Performances de la prédiction
performance(pred, measures = list(rf.costs, mmce))
#    costs        mmce 
# 1.0373006     0.3437242 

# Note : il est possible de jouer sur le seuil ici :
# pred_th <- setThreshold(pred, c(Indemne = 0.1, `Blessé léger` = 0.2, `Blessé grave` = 0.3))
# performance(pred_th, measures = list(rf.costs, mmce))

# Confusion matrix
calculateConfusionMatrix(pred, relative = FALSE, sums = FALSE, set = "both")

# Analyse du modèle -------------------------------------------------------

# Importance des variables
imp <- getFeatureImportance(modele)
imp <- t(imp$res)

# Taux de bien classés par catégorie d'usagers
bc <- data.frame(test_data2018 %>% select(catu, grav), 
                 pred$data[,c("truth", "response")])

bc %>% 
  group_by(catu) %>% 
  mutate(n_catu = n()) %>% 
  filter(truth != response) %>% 
  summarize(mal_classés = n(),
            n_catu = max(n_catu),
            taux_mal_classés = round(mal_classés/n_catu*100,1))

# Erreur de généralisation par catégorie usager ---------------------------

# Learner 
rf.learner <- makeLearner("classif.ranger",
                          predict.type = "prob", 
                          importance = c("permutation"))

# Train tasks
cond_train <- train_data2017 %>% 
  filter(catu == 'Conducteur') %>% 
  select(-catu, -place, -loc_pieton)
trainTask_cond <- makeClassifTask(data = cond_train, target = "grav")

pass_train <- train_data2017 %>% 
  filter(catu == 'Passager') %>% 
  select(-catu, -loc_pieton)
trainTask_pass <- makeClassifTask(data = pass_train, target = "grav")

piet_train <- train_data2017 %>% 
  filter(catu == 'Piéton') %>% 
  select(-catu, - place, - presence_PL)
trainTask_piet <- makeClassifTask(data = piet_train, 
                                 target = "grav")

# Train models
parallelStartSocket(5)
model_cond <- train(rf.learner, trainTask_cond)
model_pass <- train(rf.learner, trainTask_pass)
model_piet <- train(rf.learner, trainTask_piet)
parallelStop()

# Analyse des modèles -----------------------------------------------------

# Conducteurs
cond_test <- test_data2018 %>% 
  filter(catu == 'Conducteur') %>% 
  select(-catu, -place, -loc_pieton)

pred_cond <- predict(model_cond, newdata = cond_test)
calculateConfusionMatrix(pred_cond, relative = FALSE, sums = FALSE, set = "both")
performance(pred_cond, measures = list(rf.costs, mmce))
#    costs       mmce 
# 0.9708267    0.3105992 

imp_cond <- getFeatureImportance(model_cond)
imp_cond <- t(imp_cond$res)

# Passagers
pass_test <- test_data2018 %>% 
  filter(catu == 'Passager') %>% 
  select(-catu, -loc_pieton)

pred_pass <- predict(model_pass, newdata = pass_test)
calculateConfusionMatrix(pred_pass, relative = FALSE, sums = FALSE, set = "both")
performance(pred_pass, measures = list(rf.costs, mmce)) 
#    costs      mmce 
# 1.3186556   0.4787812 

imp_pass <- getFeatureImportance(model_pass)
imp_pass <- t(imp_pass$res)

# Piétons
piet_test <- test_data2018 %>% 
  filter(catu == 'Piéton') %>% 
  select(-catu, -place)

pred_piet <- predict(model_piet, newdata = piet_test)
calculateConfusionMatrix(pred_piet, relative = FALSE, sums = FALSE, set = "both")
performance(pred_piet, measures = list(rf.costs, mmce)) 
#    costs      mmce 
# 0.9891490   0.3352758 

imp_piet <- getFeatureImportance(model_piet)
imp_piet <- t(imp_piet$res)

# Passage en production ---------------------------------------------------

# Entrainement des modèles sur 2017 + 2018
all_data <- union_all(train_data2017, test_data2018)
all_data_cond <- all_data %>% filter(catu == "Conducteur") %>% select(-catu, -place, -loc_pieton)
all_data_pass <- all_data %>% filter(catu == "Passager") %>% select(-catu, -loc_pieton)
all_data_piet <- all_data %>% filter(catu == "Piéton") %>% select(-catu, -place)

trainTask_cond <- makeClassifTask(data = all_data_cond, target = "grav")
trainTask_pass <- makeClassifTask(data = all_data_pass, target = "grav")
trainTask_piet <- makeClassifTask(data = all_data_piet, target = "grav")

parallelStartSocket(5)
model_cond <- train(learner = rf.learner, task = trainTask_cond)
model_pass <- train(learner = rf.learner, task = trainTask_pass)
model_piet <- train(learner = rf.learner, task = trainTask_piet)
parallelStop()

# Sauvegarder les modèles sélectionnés
saveRDS(model_cond, "outputs/rf_conducteurs.RDS")
saveRDS(model_pass, "outputs/rf_passagers.RDS")
saveRDS(model_piet, "outputs/rf_pietons.RDS")


