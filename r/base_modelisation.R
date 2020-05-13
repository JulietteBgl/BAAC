
# Setup -------------------------------------------------------------------

source("r/data.R")

# Version 1 ---------------------------------------------------------------

# par accidents
# on ne garde que le blessé le plus grave par accident

global_acc <- usag %>% 
  inner_join(caract) %>% 
  mutate(grav = factor(grav, levels = c("Tué", "Blessé hospitalisé", "Blessé léger", "Indemne")),
         int = if_else(int != "Hors intersection", "Intersection", "Hors intersection"),
         zone = if_else(dep %in% c(75, 77, 78, 91, 92, 93, 94, 95), "IDF", "Province")
  ) %>% 
  arrange(Num_Acc, grav) %>% 
  group_by(Num_Acc) %>% 
  mutate(nb_pers_impliquées = n()) %>% 
  mutate(rank = seq(1:n())) %>% 
  filter(rank == 1) %>% 
  rename(max_grav = grav) %>% 
  inner_join(vehic) %>% 
  inner_join(lieux) %>% 
  mutate(age = 2017 - an_nais,
         groupe_age = case_when(
           age <= 16 ~ "0-16",
           age > 16 & age <=25 ~ "17-25",
           age > 25 & age <=35 ~ "26-35",
           age > 35 & age <=45 ~ "36-45",
           age > 45 & age <=55 ~ "46-55",
           age > 55 & age <=65 ~ "56-65",
           age > 65 ~ "> 65"),
         obs = if_else(!is.na(obs), "Obstacle", "Pas d'obstacle"),
         plan = if_else(plan != "Partie rectiligne", "En courbe", "Partie rectiligne"),
         catv = case_when(
           catv %in% c("Tramway", "Autobus", "Train", "Autocar") ~ "Transport en commun",
           stri_detect_fixed(catv, "Scooter") ~ "2 roues motorisé",
           stri_detect_fixed(catv, "Motocyclette") ~ "2 roues motorisé",
           catv == "Cyclomoteur < 50cm3" ~ "2 roues motorisé",
           stri_detect_fixed(catv, "Tracteur") ~ "Tracteur",
           stri_detect_fixed(catv, "PL") ~ "Engin spécial",
           catv == "Engin spécial" ~ "Engin spécial",
           stri_detect_fixed(catv, "Quad") ~ "Quad",
           catv %in% c("VL seul", "Voiturette") ~ "Voiture",
           stri_detect_fixed(catv, "VU") ~ "Véhicule utilitaire",
           catv == "Bicyclette" ~ "Vélo",
           catv == "Autre véhicule" ~ "Autre véhicule"),
         prof = if_else(prof %in% c("Bas de côte", "Sommet de côte", "Pente"), "Pente", "Plat")
  ) %>% 
  filter(catv != 'Quad' &
           !is.na(atm) &
           !is.na(surf) &
           !is.na(prof) &
           !is.na(plan) &
           !is.na(groupe_age) &
           #focus sur les routes
           catr %in% c("Autoroute", "Route Départementale", "Voie Communale", "Route Nationale")
  ) %>% 
  ungroup() 

# write csv
write.csv(global_acc, "outputs/accidents_2017_alldata.csv")

# Version 2 ---------------------------------------------------------------

# par individus

global_ind <- usag %>% 
  inner_join(vehic) %>% 
  inner_join(lieux) %>% 
  inner_join(caract) %>%
  mutate(int = if_else(int != "Hors intersection", "Intersection", "Hors intersection"),
         zone = if_else(dep %in% c(75, 77, 78, 91, 92, 93, 94, 95), "IDF", "Province"),
         age = 2017 - an_nais,
         groupe_age = case_when(
           age <= 16 ~ "0-16",
           age > 16 & age <=25 ~ "17-25",
           age > 25 & age <=35 ~ "26-35",
           age > 35 & age <=45 ~ "36-45",
           age > 45 & age <=55 ~ "46-55",
           age > 55 & age <=65 ~ "56-65",
           age > 65 ~ "> 65"),
         obs = if_else(!is.na(obs), "Obstacle", "Pas d'obstacle"),
         plan = if_else(plan != "Partie rectiligne", "En courbe", "Partie rectiligne"),
         catv = case_when(
           catv %in% c("Tramway", "Autobus", "Train", "Autocar") ~ "Transport en commun",
           stri_detect_fixed(catv, "Scooter") ~ "2 roues motorisé",
           stri_detect_fixed(catv, "Motocyclette") ~ "2 roues motorisé",
           catv == "Cyclomoteur < 50cm3" ~ "2 roues motorisé",
           stri_detect_fixed(catv, "Tracteur") ~ "Tracteur",
           stri_detect_fixed(catv, "PL") ~ "Engin spécial",
           catv == "Engin spécial" ~ "Engin spécial",
           stri_detect_fixed(catv, "Quad") ~ "Quad",
           catv %in% c("VL seul", "Voiturette") ~ "Voiture",
           stri_detect_fixed(catv, "VU") ~ "Véhicule utilitaire",
           catv == "Bicyclette" ~ "Vélo",
           catv == "Autre véhicule" ~ "Autre véhicule"),
         prof = if_else(prof %in% c("Bas de côte", "Sommet de côte", "Pente"), "Pente", "Plat")
  ) %>% 
  filter(catv != 'Quad' &
           !is.na(atm) &
           !is.na(surf) &
           !is.na(prof) &
           !is.na(plan) &
           !is.na(groupe_age) &
           #focus sur les routes
           catr %in% c("Autoroute", "Route Départementale", "Voie Communale", "Route Nationale")
  ) 

# write csv
write.csv(global_ind, "outputs/individus_2017_alldata.csv")
