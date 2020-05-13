usag2017_enrichi <- usag2017 %>% 
                    left_join(caract2017, by = c("Num_Acc" = "Num_Acc")) %>% 
                    left_join(lieux2017, by = c("Num_Acc" = "Num_Acc")) %>% 
                    left_join(vehic2017, by = c("Num_Acc" = "Num_Acc", "num_veh" = "num_veh"))

chisq.test(usag2017_enrichi$catv,usag2017_enrichi$an_nais)
usag2017_enrichi$catv222 <- usag2017_enrichi$catv
  table(usag2017_enrichi$catv222)
  usag2017_enrichi$catv222 <- fct_recode(usag2017_enrichi$catv222, "AAAAAA" = "Autobus")
table(usag2017_enrichi$catv222)
levels(usag2017_enrichi$catv222)
  comptage <- usag2017_enrichi %>%
  group_by(catv) %>%
  summarise(nb = n())

comptage <- usag2017_enrichi %>%
  group_by(catv, place) %>%
  summarise(nb = n())

comptage <- usag2017_enrichi %>%
  group_by(catv, equipement_secu) %>%
  summarise(nb = n())

table(usag2017_enrichi$catv)
write.csv2(x = usag2017_enrichi, file = "usag2017_enrichi.csv", row.names=FALSE, quote=FALSE, sep=";")

a <- usag2017_enrichi %>%
    filter(Num_Acc == '201700000049')

summary(usag2017_enrichi$grav)
library(caret)
glfit<-glm(grav~catu, data = usag2017_enrichi, family = 'binomial')
summary(glfit)

https://rstudio-pubs-static.s3.amazonaws.com/261616_3097bfd3aa4341faafede5ed2ca7bb39.html
https://rstudio-pubs-static.s3.amazonaws.com/456044_9c275b0718a64e6286751bb7c60ae42a.html
xgboost

Let’s evaluate 5 different algorithms:
  
  Linear Discriminant Analysis (LDA) Classification and Regression Trees (CART). k-Nearest Neighbors (kNN). Support Vector Machines (SVM) with a linear kernel. Random Forest (RF)


voir lda analyse discriminante
https://rpubs.com/StephanieStallworth/269560


model19_predict_proba <- predict(glfit, newdata = usag2017_enrichi, type="response")
https://rpubs.com/ChristianLopezB/Supervised_Machine_Learning
https://neurospection.netlify.app/post/machine-learning-basics-with-caret/
library(pROC)
roc_obj <- roc(training_test$Binar.OP130_Resultat_Global_v,rf2_model_200_predict_proba[,2]) #objet roc
print(roc_obj$auc) #aire sous la courbe
table
mat <- confusionMatrix(glfit, reference=usag2017_enrichi$grav) #Y réel doit être en facteur as.factor() )
