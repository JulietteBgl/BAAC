# Set-up ------------------------------------------------------------------

source("r/data.R")
library("FactoMineR")
library("factoextra")

# ACM ---------------------------------------------------------------------

# Caractéristiques de l'accident ------------------------------------------

# Caractéristiques de l'accident si au moins un piéton est impliqué

# Préparation de la donnée
pietons <- usag %>% 
  filter(catu == "Piéton") %>% 
  distinct(Num_Acc)

caract_pieton <- usag %>% 
  inner_join(caract) %>% 
  filter(Num_Acc %in% pietons$Num_Acc) %>% 
  mutate(zone = if_else(dep %in% c(75, 77, 78, 91, 92, 93, 94, 95), "IDF", "Province"),
         int = if_else(int != "Hors intersection", "Intersection", "Hors intersection")) %>% 
  select(lum, agg, int, atm, zone, grav)

# acm
acm <- MCA(caract_pieton, quali.sup = 6, graph = F)
plot(acm, 
     invisible = 'ind')

# Gravité de l'accident expliqué par ses caractéristiques

# Préparation de la donnée
caract_grav <- usag %>% 
  inner_join(caract) %>% 
  mutate(grav = factor(grav, levels = c("Tué", "Blessé hospitalisé", "Blessé léger", "Indemne")),
         int = if_else(int != "Hors intersection", "Intersection", "Hors intersection"),
         zone = if_else(dep %in% c(75, 77, 78, 91, 92, 93, 94, 95), "IDF", "Province")
         ) %>% 
  arrange(Num_Acc, grav) %>% 
  group_by(Num_Acc) %>% 
  mutate(rank = seq(1:n())) %>% 
  filter(rank == 1) %>% 
  ungroup() %>% 
  select(lum, agg, int, atm, zone, grav)

# acm
acm <- MCA(caract_grav, quali.sup = 6, graph = F)
plot(acm, 
     invisible = 'ind')

# Lieu de l'accident ------------------------------------------------------

# Localisation de l'accident si au moins un piéton est impliqué

# Préparation de la donnée

lieux_pieton <- usag %>% 
  inner_join(lieux) %>% 
  filter(Num_Acc %in% pietons$Num_Acc) %>% 
  mutate(plan = if_else(plan != "Partie rectiligne", "En courbe", "Partie rectiligne")) %>% 
  select(catr, #circ, 
         prof, plan, surf, grav)

# acm
acm <- MCA(lieux_pieton, quali.sup = 5, graph = F)
plot(acm, 
     invisible = 'ind')

# Gravité de l'accident expliqué par sa localisation

# Préparation de la donnée
lieux_grav <- usag %>% 
  inner_join(lieux) %>% 
  mutate(grav = factor(grav, levels = c("Tué", "Blessé hospitalisé", "Blessé léger", "Indemne")),
         plan = if_else(plan != "Partie rectiligne", "En courbe", "Partie rectiligne")) %>% 
  arrange(Num_Acc, grav) %>% 
  group_by(Num_Acc) %>% 
  mutate(rank = seq(1:n())) %>% 
  filter(rank == 1) %>% 
  ungroup() %>% 
  select(catr, #circ, 
         prof, plan, surf, grav)

# acm
acm <- MCA(lieux_grav, quali.sup = 5, graph = F)
plot(acm, 
     invisible = 'ind')

# Véhicules ---------------------------------------------------------------

# Caractéristiques du véhicule si au moins un piéton est impliqué

# Préparation de la donnée
vehic_pieton <- usag %>% 
  inner_join(vehic) %>% 
  filter(Num_Acc %in% pietons$Num_Acc) %>% 
  mutate(obs = if_else(!is.na(obs), "Obstacle", "Pas d'obstacle"),
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
           catv == "Autre véhicule" ~ "Autre véhicule")
         ) %>% 
  # suppression des quads car comportement particulier
  filter(catv != 'Quad') %>% 
  select(catv, obs, grav)

# acm
acm <- MCA(vehic_pieton, quali.sup = 3, graph = F)
plot(acm, 
     invisible = 'ind')

# Gravité de l'accident expliqué par les caractéristiques du véhicule

# Préparation de la donnée
vehic_grav <- usag %>% 
  inner_join(vehic) %>% 
  mutate(grav = factor(grav, levels = c("Tué", "Blessé hospitalisé", "Blessé léger", "Indemne")),
         obs = if_else(!is.na(obs), "Obstacle", "Pas d'obstacle"),
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
           catv == "Autre véhicule" ~ "Autre véhicule")
         ) %>% 
  arrange(Num_Acc, grav) %>% 
  group_by(Num_Acc) %>% 
  mutate(rank = seq(1:n())) %>% 
  filter(rank == 1) %>% 
  ungroup() %>% 
  filter(catv != 'Quad') %>%
  select(catv, obs, grav)

# acm
acm <- MCA(vehic_grav, quali.sup = 3, graph = F)
plot(acm, 
     invisible = 'ind')

# Usagers -----------------------------------------------------------------

usagers <- usag %>% 
  mutate(age = 2018 - an_nais,
         groupe_age = case_when(
           age <= 10 ~ "0-10",
           age > 10 & age <=20 ~ "11-20",
           age > 20 & age <=30 ~ "21-30",
           age > 30 & age <=40 ~ "31-40",
           age > 40 & age <=50 ~ "41-50",
           age > 50 & age <=60 ~ "51-60",
           age > 60 & age <=70 ~ "61-70",
           age > 70 & age <=80 ~ "71-80",
           age > 80 & age <=90 ~ "81-90",
           age > 90 ~ ">90")) %>% 
  select(catu, sexe, trajet, groupe_age, utilisation_equipement_secu, grav)

# acm
acm <- MCA(usagers, 
           quali.sup = 6, 
           graph = F)
plot(acm, 
     quali.sup = 6, 
     invisible = 'ind')

# Global ------------------------------------------------------------------

# Typologies d'accidents - on ne garde que le blessé le plus grave par accident
global_acc <- usag %>% 
  inner_join(caract) %>% 
  mutate(grav = factor(grav, levels = c("Tué", "Blessé hospitalisé", "Blessé léger", "Indemne")),
         int = if_else(int != "Hors intersection", "Intersection", "Hors intersection"),
         zone = if_else(dep %in% c(75, 77, 78, 91, 92, 93, 94, 95), "IDF", "Province")
  ) %>% 
  arrange(Num_Acc, grav) %>% 
  group_by(Num_Acc) %>% 
  mutate(rank = seq(1:n())) %>% 
  filter(rank == 1) %>% 
  inner_join(vehic) %>% 
  inner_join(lieux) %>% 
  mutate(age = 2018 - an_nais,
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
  ungroup() %>%
  select(lum, 
         agg, 
         int, 
         atm, 
         zone, 
         #catu, 
         sexe, 
         #trajet, 
         #groupe_age, 
         #utilisation_equipement_secu,
         catr,
         #prof, 
         #plan, 
         #surf, 
         catv, 
         #obs, 
         grav)


# acm
acm <- MCA(global_acc, 
           quali.sup = 9, 
           graph = F)

test <- acm$var$contrib

plot(acm, 
     invisible = 'ind')


# Typologie d'individus

global_ind <- usag %>% 
  inner_join(vehic) %>% 
  inner_join(lieux) %>% 
  inner_join(caract) %>%
  mutate(int = if_else(int != "Hors intersection", "Intersection", "Hors intersection"),
         zone = if_else(dep %in% c(75, 77, 78, 91, 92, 93, 94, 95), "IDF", "Province"),
         age = 2018 - an_nais,
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
  select(lum, 
         agg, 
         int, 
         atm, 
         zone, 
         sexe, 
         #trajet, 
         groupe_age, 
         #utilisation_equipement_secu,
         catr,
         #prof, 
         #plan, 
         #surf, 
         # catv, 
         #obs, 
         grav,
         catu)

# acm
acm <- MCA(global_ind, 
           quali.sup = 10, 
           graph = F)

test <- acm$var$contrib

plot(acm, 
     invisible = 'ind')
