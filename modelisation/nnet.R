# nnet (Réseau de neurones)--------------------------------------------------------------

# Predictions
pred <- predict(model.nnet, newdata = test_data2018)

# Confusion matrix
calculateConfusionMatrix(pred, relative = FALSE, sums = FALSE, set = "both")

# Performances de la prédiction
performance(pred, measures = mmce) # Taux d'erreur : 0.3587059
performance(pred, measures = acc) # Accuracy : 0.6412941

#' Résultats
#' taux bien classés = 64.1%, taux mal classés = 35.9%
#' Tué mal prédits, se retrouvent souvent en blessé hospitalisé

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

# Utilisation d'une matrice de coûts --------------------------------------

#' L'accuracy n'est pas le principal KPI à prendre en compte.
#' L'objectif du modèle est de prévoir correctement le besoin de matériel sur le lieu d'intervention.
#' Ainsi, nous souhaitons principalement optimiser le bon classement des blessés hospitalisés qui ont besoin
#' de plus de matériel d'intervention. A l'inverse, nous n'accordons que peu d'importance à un blessé prédit 
#' léger alors qu'il est indemne, par exemple. 
#' Pour cela, nous décidons de mettre en place une matrice de coûts :

costs = matrix(c(0, 1, 5, 1, # true : indemne
                 5, 0, 1, 5, # true : Blessé léger
                 10, 5, 0, 10, # true : blessé hospitalisé
                 1, 5, 5, 0), # true : tué
               nrow = 4, 
               byrow = T) 
colnames(costs) = rownames(costs) = getTaskClassLevels(trainTask)

# Création de la mesure du coût
rf.costs = makeCostMeasure(costs = costs,
                           best = 0, worst = 10)

# Calcul des seuils : 1/(average costs of true classes)
th = 1/rowSums(costs)
names(th) = getTaskClassLevels(trainTask)
th

pred.th = setThreshold(pred, threshold = th)

# Performance
performance(pred, measures = list(rf.costs, mmce)) #costs : 1.5276826 / mmce : 0.3587059
performance(pred.th, measures = list(rf.costs, mmce)) # costs : 1.3074527 / mmce : 0.4245982 
# Diminution des coûts, augmentation du taux d'erreur : il faut trouver un équilibre.

# Matrice de confusion
calculateConfusionMatrix(pred, relative = FALSE, sums = FALSE, set = "both")
calculateConfusionMatrix(pred.th, relative = FALSE, sums = FALSE, set = "both")



