
# Setup -------------------------------------------------------------------

source("r/data.R")

# Version 1 ---------------------------------------------------------------

# par accidents
# on ne garde que le blessé le plus grave par accident

global_acc <- usag %>% 
  inner_join(caract) %>% 
  mutate(grav = factor(grav, levels = c("Tué", "Blessé hospitalisé", "Blessé léger", "Indemne")), #pas de na
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
         plan = ifelse(plan != "Partie rectiligne" & !is.na(plan), 
                        yes = "En courbe", 
                        no = as.character(plan)
                        ), 
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
         prof = ifelse(prof != "Plat" & !is.na(prof), 
                        yes = "Pente", 
                        no = as.character(prof)) 
  ) %>% 
  select(-rank)

names(global_acc)
sapply(global_acc, function(x) sum(is.na(x)))
prop.table(table(global_acc$plan, useNA = "always"))*100
prop.table(table(global_acc$prof, useNA = "always"))*100

# write csv
write.csv(global_acc, "outputs/accidents_2017_alldata.csv")

# Version 2 ---------------------------------------------------------------

# par individus

global_ind <- usag %>% 
  left_join(vehic) %>% 
  left_join(lieux) %>% 
  left_join(caract) %>%
  group_by(Num_Acc) %>% 
  mutate(nb_pers_impliquées = n()) %>%
  ungroup() %>% 
  mutate(int = if_else(int != "Hors intersection", true = "Intersection", false = "Hors intersection"), #pas de na
         zone = if_else(dep %in% c(75, 77, 78, 91, 92, 93, 94, 95), "IDF", "Province"),
         age = 2017 - an_nais,
         groupe_age = case_when(
           age <= 16 ~ "0-16",
           age > 16 & age <=21 ~ "17-21",
           age > 21 & age <=35 ~ "22-35",
           age > 35 & age <=45 ~ "36-45",
           age > 45 & age <=55 ~ "46-55",
           age > 55 & age <=70 ~ "56-70",
           age > 70 ~ "> 70"),
         obs = if_else(!is.na(obs), "Obstacle", "Pas d'obstacle"),
         plan = ifelse(plan != "Partie rectiligne" & !is.na(plan), 
                              yes = "En courbe", 
                              no = as.character(plan)
         ),
         cat_vehic = case_when(
           catv == "Bicyclette" ~ "Vélo",
           stri_detect_fixed(catv, "Scooter") & catv != "Scooter > 125 cm3" ~ "2 roues peu puissants",
           catv == "Cyclomoteur < 50cm3" ~ "2 roues peu puissants",
           stri_detect_fixed(catv, "Motocyclette") & catv != "Motocyclette > 125 cm3" ~ "2 roues peu puissants",
           catv %in% c("Motocyclette > 125 cm3", "Scooter > 125 cm3") ~ "2 roues > 125 cm3",
           catv %in% c("VL seul", "Voiturette", "VU") ~ "Voiture",
           stri_detect_fixed(catv, "VU ") ~ "Voiture",
           stri_detect_fixed(catv, "PL") ~ "Poids lourds",
           catv == "Engin spécial" ~ "Poids lourds",
           stri_detect_fixed(catv, "Quad") ~ "Autre véhicule",
           stri_detect_fixed(catv, "Tracteur") ~ "Autre véhicule",
           catv %in% c("Tramway", "Autobus", "Train", "Autocar") ~ "Autre véhicule",
           catv == "Autre véhicule" ~ "Autre véhicule"
           ),
         prof = ifelse(prof != "Plat" & !is.na(prof), 
                       yes = "Pente", 
                       no = as.character(prof)),
         trajet = case_when(
           trajet %in% c("Domicile – travail", "Utilisation professionnelle") ~ "Pro",
           trajet %in% c("Domicile – école", "Courses – achats", "Promenade – loisirs", "Autre") ~ "Non Pro"
         ),
         lum = ifelse(lum == "Plein jour", "Jour", 
                          ifelse(lum == "Nuit avec éclairage public allumé", "Nuit avec éclairage",
                                                   "Nuit sans éclairage")),
         meteo = ifelse(atm == "Normale", "Condition météo normale", "Mauvaise condition météo"),
         surf = ifelse(surf != "Normale", "Surface glissante", "Surface normale"),
         catu = ifelse(
           catu == "Piéton en roller ou en trottinette", "Piéton", as.character(catu)
         ),
         choc = case_when(
           stri_detect_fixed(choc, "Avant") ~ "Avant", 
           stri_detect_fixed(choc, "Arrière") ~ "Arrière",
           stri_detect_fixed(choc, "Côté") ~ "Côtés", 
           stri_detect_fixed(choc, "multiples") ~ "Multiples"
         ),
         collision = case_when(
           col == "Deux véhicules - frontale" ~ "Deux véhicules - frontale & autre",
           col == "Autre collision" ~ "Deux véhicules - frontale & autre",
           col == "Deux véhicules – par le coté" ~ "Deux véhicules",
           col == "Deux véhicules – par l’arrière" ~ "Deux véhicules",
           col == "Sans collision" ~ "Sans collision",
           stri_detect_fixed(col, "Trois") ~ "Trois véhicules"
         ),
         cat_route = ifelse( 
           test = catr %in% c("Hors réseau public", 
                       "Parc de stationnement ouvert à la circulation publique", 
                       "autre"),
           yes = "Autre",
           no = as.character(catr)
         )) %>% 
   filter(gps == "Métropole") %>% 
  select(-place, 
         -locp, -actp, -etatp, -an_nais, -num_veh, -equipement_secu, -senc, -occutc, -obs, -obsm, -manv,
         -voie, -v1, -v2, -pr, -pr1, -vosp, -lartpc, -larrout, -infra, -situ, -env1, -an, -mois, -jour, -hrmn, -com, 
         -adr, -gps, -circ, -catv, -atm, -col, -catr) 
           
         
dim(global_ind)
summary(global_ind)

# write csv
write.csv(global_ind, "outputs/individus_2017_alldata.csv", row.names = F)

# check csv
test <- read.csv("outputs/individus_2017_alldata.csv")


