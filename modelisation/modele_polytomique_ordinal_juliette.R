# Dans notre cas, la variable à prédire est ordonnée. A ce titre la régression multinomiale
# n'est pas conseillée.
# Il faut préférer un modèle polytomique ordinal. Possibilité d'utiliser :
# > la fonction polr du package MASS
# > la fonction vglm du package VGAM (fonctionne à la fois pour les modèle polytomiques ordonnés
# et multinomiaux.

# Librairies --------------------------------------------------------------

library("dplyr") # dataset manipulation
library("tidylog") # provide feedback about dplyr operations
library("tidyr") # clean datasets
library("zoo") # missing values imputation
library("MASS")
select <- dplyr::select # https://medium.com/@HollyEmblem/handling-dplyr-and-mass-select-clashes-7c88258fd9d0

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

levels(test_data2018$cat_route)
levels(train_data2017$cat_route)

# clean env
rm(cleaning, gestion_na, Mode)


# Mise en place -----------------------------------------------------------

# Sélection des variables non colinéaires
var <- c("grav", "nb_pers_impl", "age", "perc_acc_av_tx_alcool_positif", "catu",
         "perc_acc_mortel_av_tx_alcool_positif","sexe", #"place", 
         "utilisation_equipement_secu", "agg", "cat_vehic", "collision", "cat_route", 
         "loc_pieton", "presence_PL")

train_data2017 <- train_data2017 %>% 
  select(all_of(var))

# Ordered logistic regression - toutes les données ------------------------

# Ici nous allons faire une cross validation holdout et non k-fold
train_data2017 <- train_data2017[sample(nrow(train_data2017)),]
n <- round(nrow(train_data2017)*0.7)
train <- train_data2017[1:n,]
validation <- train_data2017[-(1:n),]

# Train model
mod_olr <- polr(grav ~ ., 
            data = train)

# Validate model
pred <- predict(object = mod_olr, 
        newdata = validation)

mat <- data.frame(catu = validation$catu, true = validation$grav, pred)

# Accuracy 
mat %>% 
  filter(true == pred) %>% 
  summarise(perc = n() / nrow(mat) * 100) # 57% de bien classés

# MMCE
mat %>% 
  filter(true != pred) %>% 
  summarise(perc = n() / nrow(mat) * 100) # 43% de mal classés

# MMCE par catégorie usagers
mat %>% 
  group_by(catu) %>% 
  mutate(n_catu = n()) %>% 
  filter(pred != true) %>% 
  summarize(mal_classés = n(),
            n_catu = max(n_catu),
            taux_mal_classés = round(mal_classés/n_catu*100,1))

# Ordered logistic regression - conducteurs -------------------------------

train_cond <- train %>% filter(catu == "Conducteur")
validation_cond <- validation %>% filter(catu == "Conducteur")

mod_olr_cond <- polr(grav ~ ., 
                     data = train_cond %>% select(-catu, -loc_pieton, - utilisation_equipement_secu))

pred_cond <- predict(mod_olr_cond, 
                     newdata = validation_cond %>% select(-catu, -loc_pieton, -utilisation_equipement_secu))

mat_cond <- data.frame(true = validation_cond$grav, pred = pred_cond)

# Accuracy 
mat_cond %>% 
  filter(true == pred) %>% 
  summarise(perc = n() / nrow(mat_cond) * 100) # 60.12049

# Taux de mal classés
mat_cond %>% 
  filter(true != pred) %>% 
  summarise(perc = n() / nrow(mat_cond) * 100) # 39.87951


# Calcul du coût

# Utilisation d'une matrice de coûts
matrice_cout = matrix(c(0, 1, 4,  # true : indemne
                 3, 0, 3,  # true : Blessé léger
                 5, 3, 0),  # true : blessé grave
               nrow = 3, 
               byrow = T)

matrice_confusion_cond <- table(validation_cond$grav, pred_cond)
matrice_mc_pond_cond <- matrice_confusion_cond * matrice_cout
sum_mc_coef_cond <- matrice_mc_pond_cond[2,1] + matrice_mc_pond_cond[3,1] +
  matrice_mc_pond_cond[1,2] + matrice_mc_pond_cond[3,2] +
  matrice_mc_pond_cond[1,3] + matrice_mc_pond_cond[2,3]
bien_classes_cond <- matrice_confusion_cond[1,1] + matrice_confusion_cond[2,2] + matrice_confusion_cond[3,3]
mal_classes_cond <- matrice_confusion_cond[2,1] + matrice_confusion_cond[3,1] +
  matrice_confusion_cond[1,2] + matrice_confusion_cond[3,2] +
  matrice_confusion_cond[1,3] + matrice_confusion_cond[2,3]
# cost cond
sum_mc_coef_cond / (bien_classes_cond + mal_classes_cond) #1.149657

# Ordered logistic regression - passagers ---------------------------------

train_pass <- train %>% filter(catu == "Passager")
validation_pass <- validation %>% filter(catu == "Passager")

mod_olr_pass <- polr(grav ~ ., 
                     data = train_pass %>% select(-loc_pieton, -catu, -utilisation_equipement_secu
                     ))
pred_pass <- predict(object = mod_olr_pass, 
                     newdata = validation_pass %>% select(-loc_pieton, -catu, -utilisation_equipement_secu
                     ))

mat_pass <- data.frame(true = validation_pass$grav, pred = pred_pass)

# Accuracy
mat_pass %>% 
  filter(true == pred) %>% 
  summarise(perc = n() / nrow(mat_pass) * 100) #45%

# Taux de mal classés
mat_pass %>% 
  filter(true != pred) %>% 
  summarise(perc = n() / nrow(mat_pass) * 100) # 54.77696

# Calcul du coût
matrice_confusion_pass <- table(validation_pass$grav, pred_pass)
matrice_mc_pond_pass <- matrice_confusion_pass * matrice_cout
sum_mc_coef_pass <- matrice_mc_pond_pass[2,1] + matrice_mc_pond_pass[3,1] +
  matrice_mc_pond_pass[1,2] + matrice_mc_pond_pass[3,2] +
  matrice_mc_pond_pass[1,3] + matrice_mc_pond_pass[2,3]
bien_classes_pass <- matrice_confusion_pass[1,1] + matrice_confusion_pass[2,2] + matrice_confusion_pass[3,3]
mal_classes_pass <- matrice_confusion_pass[2,1] + matrice_confusion_pass[3,1] +
  matrice_confusion_pass[1,2] + matrice_confusion_pass[3,2] +
  matrice_confusion_pass[1,3] + matrice_confusion_pass[2,3]
# cost pass
sum_mc_coef_pass / (bien_classes_pass + mal_classes_pass) #1.327978

# Ordered logistic regression - piétons -----------------------------------

train_piet <- train %>% filter(catu == "Piéton") 
validation_piet <- validation %>% filter(catu == "Piéton") 

mod_olr_piet <- polr(grav ~ ., 
                     data = train_piet %>% select(-catu, - presence_PL))

pred_piet <- predict(object = mod_olr_piet, 
                     newdata = validation_piet %>% select(-catu, - presence_PL))

mat_piet <- data.frame(true = validation_piet$grav, pred = pred_piet)

# Accuracy
mat_piet %>% 
  filter(true == pred) %>% 
  summarise(perc = n() / nrow(mat_piet) * 100) # 67%

# Taux de mal classés
mat_piet %>% 
  filter(true != pred) %>% 
  summarise(perc = n() / nrow(mat_piet) * 100) # 33.05757

# Calcul du coût
matrice_confusion_piet <- table(validation_piet$grav, pred_piet)
matrice_mc_pond_piet <- matrice_confusion_piet * matrice_cout
sum_mc_coef_piet <- matrice_mc_pond_piet[2,1] + matrice_mc_pond_piet[3,1] +
  matrice_mc_pond_piet[1,2] + matrice_mc_pond_piet[3,2] +
  matrice_mc_pond_piet[1,3] + matrice_mc_pond_piet[2,3]
bien_classes_piet <- matrice_confusion_piet[1,1] + matrice_confusion_piet[2,2] + matrice_confusion_piet[3,3]
mal_classes_piet <- matrice_confusion_piet[2,1] + matrice_confusion_piet[3,1] +
  matrice_confusion_piet[1,2] + matrice_confusion_piet[3,2] +
  matrice_confusion_piet[1,3] + matrice_confusion_piet[2,3]
# cost piet
sum_mc_coef_piet / (bien_classes_piet + mal_classes_piet) #0.9655291


