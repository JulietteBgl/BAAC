
# Librairies --------------------------------------------------------------

library("dplyr") # dataset manipulation
library("tidylog") # provide feedback about dplyr operations
library("mlr")
library("parallelMap")

# Data --------------------------------------------------------------------

train_data2017 <- read.csv("outputs/individus_2017_alldata.csv")
train_data2017 <- train_data2017 %>% 
  mutate(dep = as.factor(dep),
         place = as.factor(place),
         grav = factor(grav, levels = c("Indemne", "Blessé léger", "Blessé hospitalisé", "Tué"))
         ) %>% 
  select(-Num_Acc, -groupe_age, -lat, -long) %>% 
  rename(nb_pers_impl = nb_pers_impliquées) 

test_data2018 <- read.csv("outputs/individus_2018_testdata.csv")
test_data2018 <- test_data2018 %>% 
  mutate(dep = as.factor(dep),
         place = as.factor(place),
         grav = factor(grav, levels = c("Indemne", "Blessé léger", "Blessé hospitalisé", "Tué"))
         ) %>%
  select(-Num_Acc, -groupe_age, -lat, -long) %>% 
  rename(nb_pers_impl = nb_pers_impliquées)

# Gestion des valeurs manquantes ------------------------------------------

# 2017
sum(is.na(train_data2017)) #64453 #296627

# remplacer les variables factor en char
for(i in 1:ncol(train_data2017)) {
  if (is.factor(train_data2017[,i]) & 
    i != which(colnames(train_data2017) == "grav")) {
    train_data2017[,i] <- as.character(train_data2017[,i])
  }
}

# correction des erreurs
train_data2017 <- train_data2017 %>% 
  mutate(
    utilisation_equipement_secu = ifelse(
    test = catu %in% c("Conducteur", "Passager") & 
      is.na(utilisation_equipement_secu),
    yes = "Non déterminable",
    no = as.character(utilisation_equipement_secu)))

table(train_data2017$utilisation_equipement_secu, 
      train_data2017$catu, 
      useNA = "always")  

# Pour les variables quali : ajout de la variable "non communiqué" 
# pour les variables quanti : imputation par la moyenne
train_data2017 <- train_data2017 %>% 
  mutate_if(is.character, ~replace(., is.na(.), "Non communiqué")) %>% 
  mutate_if(is.integer, ~replace(., is.na(.), mean(., na.rm = TRUE)))

# re transformation des données en facteurs
for(i in 1:ncol(train_data2017)) {
  if (is.character(train_data2017[,i])) {
    train_data2017[,i] <- as.factor(train_data2017[,i])
  }
}

summary(train_data2017)

# 2018

sum(is.na(test_data2018)) #54142

# remplacer les variables factor en char
for(i in 1:ncol(test_data2018)) {
  if (is.factor(test_data2018[,i]) & 
      i != which(colnames(test_data2018) == "grav")) {
    test_data2018[,i] <- as.character(test_data2018[,i])
  }
}

# correction des erreurs
test_data2018 <- test_data2018 %>% 
  mutate(
    utilisation_equipement_secu = ifelse(
      test = catu %in% c("Conducteur", "Passager") & 
        is.na(utilisation_equipement_secu),
      yes = "Non déterminable",
      no = as.character(utilisation_equipement_secu)))

table(test_data2018$utilisation_equipement_secu, 
      test_data2018$catu, 
      useNA = "always")  

# Pour les variables quali : ajout de la variable "non communiqué" 
# pour les variables quanti : imputation par la moyenne
test_data2018 <- test_data2018 %>% 
  mutate_if(is.character, ~replace(., is.na(.), "Non communiqué")) %>% 
  mutate_if(is.integer, ~replace(., is.na(.), mean(., na.rm = TRUE)))

sum(is.na(test_data2018)) #0

# re transformation des données en facteurs
for(i in 1:ncol(test_data2018)) {
  if (is.character(test_data2018[,i])) {
    test_data2018[,i] <- as.factor(test_data2018[,i])
  }
}

summary(test_data2018)

# Tasks -------------------------------------------------------------------

trainTask = makeClassifTask(data = train_data2017, target = "grav")
testTask = makeClassifTask(data = test_data2018, target = "grav")

# Learner -----------------------------------------------------------------

rf.learner <- makeLearner("classif.ranger",
                          predict.type = "prob", 
                          importance = c("permutation")
                          )

# Train model -------------------------------------------------------------

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

# confusion matrix
calculateConfusionMatrix(pred, relative = FALSE, sums = FALSE, set = "both")

# Taux de biens classés au seuil de 0.25
matrice <- pred$data[,c("truth", "response")]
matrice %>% 
  filter(truth == response) %>% 
  summarize(bien_classés = n(),
            taux_bien_classés = round(bien_classés/nrow(matrice)*100,1))

# Taux de mal classés au seuil de 0.25
matrice %>% 
  filter(truth != response) %>% 
  summarize(mal_classés = n(),
            taux_mal_classés = round(mal_classés/nrow(matrice)*100,1))

#' Résultats
#' Avec toutes les variables : taux_bien_classés = 64.1%, taux_mal_classés = 35.9%
#' Sans les variables trajet & prof, moins bien 
#' Sans la variable catu, moins bien 
#' Sans la variable nb_pers_impl, bcp moins bien
#' --> Très bonne chose d'avoir créé la variable nb_pers_impl
#' Avec ajout place véhicule, loc piéton et action piéton : taux_bien_classés = 64.2%, taux_mal_classés = 35.8%

# matrice prediction
bc <- data.frame(test_data2018 %>% select(catu, grav), matrice)

# Taux de bien classés par catégorie usager
bc %>% 
  group_by(catu) %>% 
  mutate(n_catu = n()) %>% 
  filter(truth == response) %>% 
  summarize(bien_classés = n(),
            n_catu = max(n_catu),
            taux_bien_classés = round(bien_classés/n_catu*100,1))

#' Les passagers ne sont pas classés correctement presque une fois sur 2.
#' Légère amélioration avec l'ajout de la place, mais pas suffisant.
#' Peut-être faire des groupement ?
#' Bien revoir ce qui a été amélioré par catégorie d'usager

# Taux de bien classés par catégorie usager et gravité de l'accident
bc %>% 
  group_by(catu, grav) %>% 
  mutate(n_catu_grav = n()) %>% 
  filter(truth == response) %>% 
  summarize(bien_classés = n(),
            n_catu_grav = max(n_catu_grav),
            taux_bien_classés = round(bien_classés/n_catu_grav*100,1))


# Tué pas très prédit : voir si on peut ajouter un seuil ou une pénalité ?


# Influence du seuil ------------------------------------------------------

