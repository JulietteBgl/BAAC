# CatBoost is a machine learning algorithm that uses gradient boosting on decision trees. 
# 
# Categorical features are used to build new numeric features based on categorical 
# features and their combinations. 
# 
# By default, CatBoost uses one-hot encoding for categorical features with a small amount 
# of different values in most modes.

# Before each split is selected in the tree (see Choosing the tree structure), categorical 
# features are transformed to numerical. This is done using various statistics on combinations 
# of categorical features and combinations of categorical and numerical features.


# Libraries ---------------------------------------------------------------

library("catboost") # https://ampersandacademy.com/tutorials/r-programming/install-catboost-r-package-on-mac-linux-and-windows
library("dplyr") # dataset manipulation
library("tidylog") # provide feedback about dplyr operations
library("tidyr") # clean datasets
library("zoo") # missing values imputation

# Data --------------------------------------------------------------------

# Load data
train_data2017 <- read.csv("outputs/individus_2017_alldata.csv")
test_data2018 <- read.csv("outputs/individus_2018_alldata.csv")

# Création d'une fonction de cleaning et sélection de variables
cleaning <- function(dataframe){
  dataframe <- dataframe %>% 
    mutate(dep = as.factor(dep),
           place = as.factor(place),
           grav = factor(grav, levels = c("Indemne", "Blessé léger", "Blessé hospitalisé", "Tué"))
    ) %>%
    select(-Num_Acc, -groupe_age, -lat, -long
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
  
  # correction des erreurs de codage
  dataframe <- dataframe %>%
    mutate(
      utilisation_equipement_secu = ifelse(
        test = catu %in% c("Conducteur", "Passager") &
          is.na(utilisation_equipement_secu),
        yes = "Non déterminable",
        no = as.character(utilisation_equipement_secu)
      )
    )
  
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
    group_by(cat_route, agg) %>%
    mutate(
      nbv = na.aggregate(nbv, FUN = mean),
      prof = na.aggregate(prof, FUN = Mode),
      int = na.aggregate(int, FUN = Mode)
    ) %>%
    ungroup() %>%
    group_by(cat_route, agg, catu) %>%
    mutate(collision = na.aggregate(collision, FUN = Mode)) %>%
    ungroup() %>%
    group_by(surf) %>%
    mutate(meteo = na.aggregate(meteo, FUN = Mode)) %>%
    ungroup() %>%
    group_by(meteo) %>%
    mutate(surf = na.aggregate(surf, FUN = Mode)) %>%
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

# Catboost ----------------------------------------------------------------

input <- train_data2017 %>% select(-grav)
label <- train_data2017 %>% select(grav)

cat <- sapply(train_data2017, function(x) is.factor(x))
cat <- names(cat[cat == TRUE])
cat <- which(colnames(input) %in% cat)

pool_train <- catboost.load_pool(input, 
                   label = as.numeric(label$grav) - 1, 
                   cat_features = cat)
head(pool_train)

fit_params <- list(iterations = 100,
                    loss_function = 'MultiClass'
                   # loss_function = 'Logloss',
                   # depth = 5,
                   # learning_rate = 0.03
                   )

model <- catboost.train(pool_train, params = fit_params)

# Validate model ----------------------------------------------------------

pool_test <- catboost.load_pool(test_data2018 %>% select(-grav), 
                           cat_features = cat)

pred <- catboost.predict(model, 
                         pool_test,
                         prediction_type = 'Class')
table(pred)


