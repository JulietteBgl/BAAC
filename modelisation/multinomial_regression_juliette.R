# Multinomial logistic regression is a classification method that generalizes logistic regression to 
# multiclass problems, i.e. with more than two possible discrete outcomes. That is, it is a model that 
# is used to predict the probabilities of the different possible outcomes of a categorically distributed 
# dependent variable, given a set of independent variables (which may be real-valued, binary-valued, 
# categorical-valued, etc.).
# 
# Multinomial logistic regression is known by a variety of other names, including polytomous LR, multiclass 
# LR, softmax regression, multinomial logit (mlogit), the maximum entropy (MaxEnt) classifier, and the 
# conditional maximum entropy model.

# Multinomial logistic regression is used when the dependent variable in question is nominal (equivalently 
# categorical, meaning that it falls into any one of a set of categories that cannot be ordered in any 
# meaningful way) and for which there are more than two categories. 

# Attention : dans notre cas, la variable à prédire est ordonnée. A ce titre la régression multinomiale
# n'est pas conseillée.
# Il faut préférer un modèle polytomique ordinal (ex : fonction polr du package MASS).
# A noter que la fonction vglm du package VGAM fonctionne à la fois pour les modèle polytomiques 
# ordonnés et multinomiaux.

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

levels(train_data2017$cat_route) <- levels(test_data2018$cat_route)

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

# Sur 2018, remplacer "Non communiqué" par "Autre" pour la catégorie du véhicule
test_data2018 <- test_data2018 %>% 
  mutate(cat_route = ifelse(
    cat_route == "Non communiqué",
    yes = "Autre",
    no = as.character(cat_route)
  ),
  cat_route = factor(cat_route))

# clean env
rm(cleaning, gestion_na, Mode)

# Mise en place -----------------------------------------------------------

# Sélection des variables non colinéaires
var <- c("grav", "nb_pers_impl", "age", "perc_acc_av_tx_alcool_positif", 
         "perc_acc_mortel_av_tx_alcool_positif", "sexe", "place", "utilisation_equipement_secu", 
         "agg","cat_vehic", "dep", "catu","collision", "cat_route", "loc_pieton", "presence_PL")

# Learner 
mr.learner <- makeLearner("classif.multinom",
                          predict.type = "prob"
)

getParamSet("classif.multinom")

# Régression multinomiale - toutes les données ----------------------------

# Tasks 
trainTask = makeClassifTask(data = train_data2017 %>% select(all_of(var)), target = "grav")
# testTask = makeClassifTask(data = test_data2018 %>% select(all_of(var)), target = "grav")
ln <- listLearners(trainTask)

# cross validation - resample instance
r_ins = makeResampleInstance("CV", iters = 5, task = trainTask)

# Utilisation d'une matrice de coûts
costs = matrix(c(0, 1, 4,  # true : indemne
                 3, 0, 3,  # true : Blessé léger
                 5, 3, 0),  # true : blessé grave
               nrow = 3, 
               byrow = T) 

colnames(costs) = rownames(costs) = getTaskClassLevels(trainTask)

# Création de la mesure du coût
mr.costs = makeCostMeasure(costs = costs,
                            best = 0, worst = 5)

# Train model
parallelStartSocket(5)

r = resample(learner = mr.learner, 
             task = trainTask, 
             resampling = r_ins, 
             measures = list(mr.costs, mmce), 
             show.info = FALSE)

parallelStop()
r

# Resample Result
# Task: train_data2017 %>% select(all_of(var))
# Learner: classif.multinom
# Aggr perf: costs.test.mean=1.1436592,mmce.test.mean=0.3644493
# Runtime: 47.2906

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

# Régression multinomiale - conducteurs -----------------------------------

train_cond <- train_data2017 %>% 
  filter(catu == "Conducteur") %>% 
  select(all_of(var)) %>% 
  select(-catu, -place, -utilisation_equipement_secu, -loc_pieton) 

trainTask_cond = makeClassifTask(data = train_cond, target = "grav")

# cross validation - resample instance
r_ins = makeResampleInstance("CV", iters = 5, task = trainTask_cond)

# Train model
parallelStartSocket(5)

r = resample(learner = mr.learner, 
             task = trainTask_cond, 
             resampling = r_ins, 
             measures = list(mr.costs, mmce), 
             show.info = FALSE)

parallelStop()
r

# Resample Result
# Task: train_cond
# Learner: classif.multinom
# Aggr perf: costs.test.mean=1.0946612,mmce.test.mean=0.3394503
# Runtime: 32.4218

# Régression muultinomiale - passagers ------------------------------------

train_pass <- train_data2017 %>% 
  filter(catu == "Passager") %>% 
  select(all_of(var)) %>% 
  select(-catu, -utilisation_equipement_secu, -loc_pieton)

trainTask_pass = makeClassifTask(data = train_pass, target = "grav")

# cross validation - resample instance
r_ins = makeResampleInstance("CV", iters = 5, task = trainTask_pass)

# Train model
parallelStartSocket(5)

r = resample(learner = mr.learner, 
             task = trainTask_pass, 
             resampling = r_ins, 
             measures = list(mr.costs, mmce), 
             show.info = FALSE)

parallelStop()
r

# Resample Result
# Task: train_pass
# Learner: classif.multinom
# Aggr perf: costs.test.mean=1.3413010,mmce.test.mean=0.4748580
# Runtime: 6.55872

# Régression multinomiale - piétons ---------------------------------------

train_piet <- train_data2017 %>% 
  filter(catu == "Piéton") %>% 
  select(all_of(var)) %>% 
  select(-catu, -place)

trainTask_piet = makeClassifTask(data = train_piet, target = "grav")

# cross validation - resample instance
r_ins = makeResampleInstance("CV", iters = 5, task = trainTask_piet)

# Train model
parallelStartSocket(5)

r = resample(learner = mr.learner, 
             task = trainTask_piet, 
             resampling = r_ins, 
             measures = list(mr.costs, mmce), 
             show.info = FALSE)

parallelStop()
r

# Resample Result
# Task: train_piet
# Learner: classif.multinom
# Aggr perf: costs.test.mean=0.9558901,mmce.test.mean=0.3214259
# Runtime: 3.1425