# Librairies --------------------------------------------------------------

library("dplyr") # dataset manipulation
library("tidylog") # provide feedback about dplyr operations
library("mlr") # create dummy variables
library("tidyr") # clean datasets
library("zoo") # missing values imputation
library("keras")

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


# Neural Network ----------------------------------------------------------

# Pour être compatible avec Keras, il faut coder les variables qualitatives sous forme disjonctive 
# et mettre les données d'input sous forme matricielle.

# https://towardsdatascience.com/all-about-categorical-variable-encoding-305f3361fd02

nn_recoding <- function(df) {
  df <- df %>% 
    mutate(grav = case_when(
      grav == "Indemne" ~ 1,
      grav == "Blessé léger" ~ 2,
      grav == "Blessé hospitalisé" ~ 3,
      grav == "Tué" ~ 4),
      # standardisation des variables quantitatives
      age = scale(age, center = TRUE, scale = TRUE),
      nbv = scale(nbv, center = TRUE, scale = TRUE)
      )
  
  createDummyFeatures(df %>% 
                        select(-dep))
}

train_data2017_REC <- nn_recoding(train_data2017)
test_data2018_REC <- nn_recoding(test_data2018)

test_data2018_REC <- test_data2018_REC[,names(test_data2018_REC) %in% names(train_data2017_REC)]

appX <- as.matrix(train_data2017_REC %>% select(-grav))
appY <- as.matrix(train_data2017_REC %>% select(grav))

testX <- as.matrix(test_data2018_REC %>% select(-grav))
testY <- as.matrix(test_data2018_REC %>% select(grav))

# Réseau avec une couche
mod1couche <- keras_model_sequential() %>%  # create a linear stack of layers
  layer_dense(units = 4,  activation = "softmax") # output layer

mod1couche %>% 
  compile(loss = "categorical_crossentropy", # measure mismatch between y_pred and y, calculated after each minibatch
          optimizer = "rmsprop", # network will update itself based on the training data & loss
          metrics = c("accuracy")) # measure of performance - correctly classified users

history <- mod1couche %>% 
  fit(appX, appY, epochs = 30, validation_split = 0.3)

plot(history) # on va garder 1 epoch @Mathieu : qu'est-ce que je fais de cette info ?

# # Réseau avec 2 couches
mod2couches <- keras_model_sequential() %>%  # create a linear stack of layers
  layer_dense(units = 60, activation = "relu") %>%  # input layer
  layer_dense(units = 4,  activation = "softmax")# output layer

mod2couches %>% 
  compile(loss = "categorical_crossentropy", # measure mismatch between y_pred and y, calculated after each minibatch
          optimizer = "rmsprop", # network will update itself based on the training data & loss
          metrics = c("accuracy")) # measure of performance - correctly classified fatalities

history <- mod2couches %>% 
  fit(appX, appY, epochs = 60, validation_split = 0.3)
plot(history) # #Mathieu : idem, je ne sais pas quoi faire de cette info

# Réseau avec 3 couches
mod3couches <- keras_model_sequential() %>%  # create a linear stack of layers
  layer_dense(units = 100, activation = "relu") %>%  # input layer
  layer_dense(units = 50, activation = "relu") %>% # hidden layer
  layer_dense(units = 4,  activation = "softmax") # output layer

#@Mathieu : qu'est-ce que c'est vraiment epoch ? Comment choisir les units ?

mod3couches %>% 
  compile(loss = "categorical_crossentropy", # measure mismatch between y_pred and y, calculated after each minibatch
          optimizer = "rmsprop", # network will update itself based on the training data & loss
          metrics = c("accuracy")) # measure of performance - correctly classified fatalities

history <- mod3couches %>% 
  fit(appX, appY, epochs = 60, validation_split = 0.3)
plot(history)


# Predictions -------------------------------------------------------------

pred_mod1 <- apply(predict(mod1couche, testX), 1, which.max)
pred_mod2 <- apply(predict(mod2couches, testX), 1, which.max)
pred_mod3 <- apply(predict(mod3couches, testX), 1, which.max)

res_mod1 <- as.data.frame(cbind(testY, pred_mod1))
res_mod1 %>% 
  filter(grav == pred_mod1) %>% 
  summarise(acc = n() / nrow(res_mod1)) # taux de bien classés : 30%

res_mod2 <- as.data.frame(cbind(testY, pred_mod2))
res_mod2 %>% 
  filter(grav == pred_mod2) %>% 
  summarise(acc = n() / nrow(res_mod2)) # taux de bien classés : 2%

res_mod3 <- as.data.frame(cbind(testY, pred_mod3))
res_mod3 %>% 
  filter(grav == pred_mod3) %>% 
  summarise(acc = n() / nrow(res_mod3)) # taux de bien classés : 39%


