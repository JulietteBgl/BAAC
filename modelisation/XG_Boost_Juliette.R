# Librairies --------------------------------------------------------------

library("dplyr") # dataset manipulation
library("tidylog") # provide feedback about dplyr operations
library("mlr") # interface to classification and regression techniques
library("parallelMap") # parallelization framework
library("tidyr") # clean datasets
library("zoo") # missing values imputation

# Data --------------------------------------------------------------------

# Load data
data2017 <- read.csv("outputs/individus_2017_alldata.csv")
data2018 <- read.csv("outputs/individus_2018_alldata.csv")

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
data2017 <- cleaning(data2017)
data2018 <- cleaning(data2018)

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
data2017 <- gestion_na(data2017)
data2018 <- gestion_na(data2018)

# Sur 2018, remplacer "Non communiqué" par "Autre" pour la catégorie du véhicule
data2018 <- data2018 %>% 
  mutate(cat_route = ifelse(
    cat_route == "Non communiqué",
    yes = "Autre",
    no = as.character(cat_route)
  ),
  cat_route = factor(cat_route))

# union all data
all_data <- union_all(data2017, data2018)

# clean env
rm(cleaning, gestion_na, Mode)

# Echantillonnage ---------------------------------------------------------

p <- 0.7
n <- nrow(all_data)

all_data <- all_data[sample(n),]

train_i <- all_data[1:(n*p),]
test <- all_data[-(1:(n*p)),]

# One hot encoding --------------------------------------------------------

# Plusieurs méthodes d'encodage de variable qualitatives sont détaillées sur cet article :
# https://towardsdatascience.com/all-about-categorical-variable-encoding-305f3361fd02

colClean <- function(x){ 
  colnames(x) <- gsub("é", "e", colnames(x))
  colnames(x) <- gsub("è", "e", colnames(x))
  colnames(x) <- gsub("ô", "o", colnames(x))
  colnames(x) <- gsub("ê", "e", colnames(x)) 
  colnames(x) <- gsub("...", ".", colnames(x), fixed = TRUE); x 
}

vars <- c("place", "catu", "sexe", 
          "utilisation_equipement_secu", 
          "agg", 
          "zone", 
          "cat_vehic", 
          "collision", "cat_route", "loc_pieton", 
          "presence_PL", 
          "dep",
          "lum",
          "atm",
          "int"
          )

train <- createDummyFeatures(train_i, 
                             cols = vars)
train <- colClean(train)
train$grav <- factor(as.numeric(train$grav) - 1)

test <- createDummyFeatures(test, 
                            cols = vars)
test <- colClean(test)
test$grav <- factor(as.numeric(test$grav) - 1)

# clean env
rm(vars, colClean, data2017, data2018)

# Mise en place -----------------------------------------------------------

# Learner 
getParamSet("classif.xgboost")
xgboost.learner <- makeLearner("classif.xgboost",
                               predict.type = "prob",
                               par.vals = list(
                                 objective = "multi:softprob")) 


# XG Boost - toutes les données -------------------------------------------

# Tasks 
trainTask = makeClassifTask(data = train, target = "grav")
#testTask = makeClassifTask(data = test, target = "grav")
ln <- listLearners(trainTask)

# cross validation - resample instance
r_ins = makeResampleInstance("CV", iters = 5, task = trainTask)

# Train model
parallelStartSocket(5)
r = resample(learner = xgboost.learner, 
             task = trainTask, 
             resampling = r_ins, 
             measures = list(mmce), 
             show.info = FALSE)
parallelStop()
r # mmce.test.mean=0.3578283


# Utilisation d'une matrice de coûts
costs = matrix(c(0, 1, 4,  # true : indemne
                 3, 0, 3,  # true : Blessé léger
                 5, 3, 0),  # true : blessé grave
               nrow = 3, 
               byrow = T) 

colnames(costs) = rownames(costs) = getTaskClassLevels(trainTask)

# Création de la mesure du coût
xgb.costs = makeCostMeasure(costs = costs,
                           best = NULL, worst = NULL)

# Train model
parallelStartSocket(5)

r = resample(learner = xgboost.learner, 
             task = trainTask, 
             resampling = r_ins, 
             measures = list(xgb.costs), 
             show.info = FALSE)

parallelStop()
r # costs.test.mean=1.0827682

# Taux de bien classés par catégorie d'usagers
bc <- r$pred$data[,c("id", "truth", "response")]
true <- train_i %>% 
  mutate(id = seq(1:nrow(train_i))) %>% 
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

# XG Boost - conducteurs --------------------------------------------------

# Tasks
cond_train <- train %>% 
  filter(catu.Conducteur == 1) %>% 
  select(-catu.Conducteur, -catu.Passager, -catu.Pieton,
         -place.Arriere, -place.Avant, -place.Non.communique,
         -loc_pieton.Hors.zone.pietonne, -loc_pieton.Non.communique, -loc_pieton.Zone.pietonne..dont.passage.pieton.
         )

trainTask_cond = makeClassifTask(data = cond_train, 
                                 target = "grav")

# cross validation - resample instance
r_ins = makeResampleInstance("CV", iters = 5, task = trainTask_cond)

# Train model
parallelStartSocket(5)

r = resample(learner = xgboost.learner, 
             task = trainTask_cond, 
             resampling = r_ins, 
             measures = list(xgb.costs, mmce), 
             show.info = FALSE)

parallelStop()
r

# Resample Result
# Task: cond_train
# Learner: classif.xgboost
# Aggr perf: costs.test.mean=1.0647491,mmce.test.mean=0.3235448
# Runtime: 5.87825

# Matrice de confusion
calculateConfusionMatrix(r$pred, relative = FALSE, sums = FALSE, set = "both")

# XG Boost - passagers ----------------------------------------------------

pass_train <- train %>% 
  filter(catu.Passager == 1) %>% 
  select(-catu.Conducteur, -catu.Passager, -catu.Pieton, 
         -loc_pieton.Hors.zone.pietonne, -loc_pieton.Non.communique, -loc_pieton.Zone.pietonne..dont.passage.pieton. 
  )

trainTask_pass = makeClassifTask(data = pass_train, 
                                 target = "grav")

# cross validation - resample instance
r_ins = makeResampleInstance("CV", iters = 5, task = trainTask_pass)

# Train model
parallelStartSocket(5)

r = resample(learner = xgboost.learner, 
             task = trainTask_pass, 
             resampling = r_ins, 
             measures = list(xgb.costs, mmce), 
             show.info = FALSE)

parallelStop()
r

# Resample Result
# Task: pass_train
# Learner: classif.xgboost
# Aggr perf: costs.test.mean=1.4081053,mmce.test.mean=0.4787300
# Runtime: 1.33323

# Matrice de confusion
calculateConfusionMatrix(r$pred, relative = FALSE, sums = FALSE, set = "both")

# XG Boost - piétons ------------------------------------------------------

piet_train <- train %>% 
  filter(catu.Pieton == 1) %>% 
  select(-catu.Conducteur, -catu.Passager, -catu.Pieton,  
         -place.Arriere, -place.Avant, -place.Non.communique, 
         -presence_PL.non, -presence_PL.oui)

trainTask_piet = makeClassifTask(data = piet_train, 
                                 target = "grav")

# cross validation - resample instance
r_ins = makeResampleInstance("CV", iters = 5, task = trainTask_piet)

# Train model
parallelStartSocket(5)

r = resample(learner = xgboost.learner, 
             task = trainTask_piet, 
             resampling = r_ins, 
             measures = list(xgb.costs, mmce), 
             show.info = FALSE)

parallelStop()
r

# Resample Result
# Task: piet_train
# Learner: classif.xgboost
# Aggr perf: costs.test.mean=0.9901572,mmce.test.mean=0.3347811
# Runtime: 0.563767

# Matrice de confusion
calculateConfusionMatrix(r$pred, relative = FALSE, sums = FALSE, set = "both")

