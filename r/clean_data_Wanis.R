# Librairires -------------------------------------------------------------
library("dplyr") # dataset manipulation
library("stringi") # string manipulation

# Préparation des données -------------------------------------------------

for (i in c("2017", "2018")) {
  
  # Import des fichiers -----------------------------------------------------
  
  # Caractéristiques
  caract <- read.csv(file = paste0("data/",i,"/caracteristiques-", i, ".csv"), header = TRUE, sep = ",") 
  # Véhicules
  vehic <- read.csv(file = paste0("data/",i,"/vehicules-", i, ".csv"), header = TRUE, sep = ",")
  # Usagers
  usag <- read.csv(file = paste0("data/",i,"/usagers-", i, ".csv"), header = TRUE, sep = ",")
  # Lieux
  lieux <- read.csv(file = paste0("data/",i,"/lieux-", i, ".csv"), header = TRUE, sep = ",")
  
  # 1. Data recode -------------------------------------------------------------
  
  # Caractéristiques --------------------------------------------------------
  
  # Remplacer les "\xe9" par des é (sinon bug encodage UTF8)
  caract <- caract %>% 
    mutate(adr = stri_replace_all_fixed(str = adr, 
                                        pattern = "\xe9", 
                                        replacement = "é"))
  
  # Filtre sur la métropole et recodage des codes postaux
  caract <- 
    caract %>% 
    filter(gps == "M",
           !(dep %in% c(201,202))) %>% # suppression de la Corse
    mutate(dep = stri_pad_left(str = dep, width = 3, pad = 0),
           dep = substr(dep, start = 1, stop = 2))
  
  # Transformation des variables quantitatives en facteur
  caract$Num_Acc <- factor(caract$Num_Acc)
  caract$mois <- factor(caract$mois, levels = 1:12, labels = c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre"))
  caract$lum <- factor(caract$lum, levels = c(1,2,3,4,5), labels = c("Plein jour", "Crépuscule ou aube", "Nuit sans éclairage public", "Nuit avec éclairage public non allumé", "Nuit avec éclairage public allumé"))
  caract$agg <- factor(caract$agg, levels = c(1,2), labels = c("Hors Agglo", "Agglo"))
  caract$int <- factor(caract$int, levels = c(1,2,3,4,5,6,7,8,9), labels = c("Hors intersection","Intersection en X","Intersection en T", "Intersection en Y", "Intersection à plus de 4 branches", "Giratoire", "Place", "Passage à niveau","Autre intersection"))
  caract$atm <- factor(caract$atm, levels = c(1,2,3,4,5,6,7,8,9), labels = c("Normale", "Pluie légère", "Pluie forte", "Neige - grêle", "Brouillard - fumée", "Vent fort - tempête", "Temps éblouissant", "Temps couvert", "Autre"))
  caract$col <- factor(caract$col, levels = c(1,2,3,4,5,6,7), labels = c("Deux véhicules - frontale", "Deux véhicules – par l’arrière", "Deux véhicules – par le coté","Trois véhicules et plus – en chaîne","Trois véhicules et plus - collisions multiples","Autre collision","Sans collision"))
  caract$gps <- factor(caract$gps, levels = c("M","A","G","R","Y"), labels = c("Métropole", "Antilles", "Guyane", "Réunion", "Mayotte"))
  
  # Véhicules ---------------------------------------------------------------
  
  # Transformation des variables quantitatives en facteur
  vehic$Num_Acc <- factor(vehic$Num_Acc)
  vehic$senc <- factor(vehic$senc, levels = c(1,2), labels = c("Croissant","Décroissant"))
  vehic$catv <- factor(vehic$catv, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,30,31,32,33,34,35,36,37,38,39,40,99), labels = c("Bicyclette", "Cyclomoteur < 50cm3", "Voiturette","04","05","06","VL seul", "08", "09", "VU seul 1,5T <= PTAC <= 3,5T avec ou sans remorque","11", "12", "PL seul 3,5T <PTCA <= 7,5T","PL seul > 7,5T"," PL > 3,5T + remorque", "Tracteur routier seul","Tracteur routier + semi-remorque","18","19", "Engin spécial","Tracteur agricole","Scooter < 50 cm3","Motocyclette > 50 cm3 et <= 125 cm3","Scooter > 50 cm3 et <= 125 cm3","Motocyclette > 125 cm3","Scooter > 125 cm3","Quad léger <= 50 cm3", "Quad lourd > 50 cm3", "Autobus", "Autocar", "Train", "Tramway", "Autre véhicule"))
  vehic$obs <- factor(vehic$obs, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), labels = c("Véhicule en stationnement","Arbre","Glissière métallique","Glissière béton","Autre glissière","Bâtiment, mur, pile de pont","Support de signalisation verticale ou poste d’appel d’urgence","Poteau","Mobilier urbain","Parapet","Ilot, refuge, borne haute","Bordure de trottoir","Fossé, talus, paroi rocheuse","Autre obstacle fixe sur chaussée","Autre obstacle fixe sur trottoir ou accotement","Sortie de chaussée sans obstacle"))
  vehic$obsm <- factor(vehic$obsm, levels = c(1,2,4,5,6,9), labels = c("Piéton", "Véhicule", "Véhicule sur rail", "Animal domestique", "Animal sauvage", "Autre"))
  vehic$choc <- factor(vehic$choc, levels = c(1,2,3,4,5,6,7,8,9), labels = c("Avant","Avant droit","Avant gauche","Arrière","Arrière droit","Arrière gauche","Côté droit","Côté gauche","Chocs multiples (tonneaux)"))
  vehic$manv <- factor(vehic$manv, levels =  c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), labels = c("Sans changement de direction","Même sens, même file","Entre 2 files","En marche arrière","A contresens","En franchissant le terre-plein central","Dans le couloir bus, dans le même sens","Dans le couloir bus, dans le sens inverse","En s’insérant","En faisant demi-tour sur la chaussée","Changeant de file A gauche","Changeant de file A droite","Déporté A gauche","Déporté A droite","Tournant A gauche","Tournant A droite","Dépassant A gauche","Dépassant A droite","Traversant la chaussée","Manoeuvre de stationnement","Manoeuvre d’évitement","Ouverture de porte","Arrêté (hors stationnement)","En stationnement (avec occupants)"))
  
  # Usagers -----------------------------------------------------------------
  
  # Transformation des variables quantitatives en facteur
  usag$Num_Acc <- factor(usag$Num_Acc)
  usag$catu <- factor(usag$catu, levels =  c(1,2,3,4), labels = c("Conducteur","Passager","Piéton","Piéton en roller ou en trottinette"))
  usag$grav <- factor(usag$grav, levels = c(1,2,3,4), labels = c("Indemne","Tué","Blessé hospitalisé","Blessé léger"))
  usag$sexe <- factor(usag$sexe, levels = c(1,2), labels = c("Masculin","Féminin"))
  usag$trajet <- factor(usag$trajet, levels = c(1,2,3,4,5,9), labels = c("Domicile – travail","Domicile – école","Courses – achats","Utilisation professionnelle","Promenade – loisirs","Autre"))
  
  # secu
  equipement_secu <- substr(usag$secu, start = 1, stop = 1)
  utilisation_equipement_secu <- substr(usag$secu, start = 2, stop = 2)
  
  usag <- cbind(usag, equipement_secu, utilisation_equipement_secu)
  usag <- usag %>% 
    select(-secu)
  
  usag$equipement_secu <- factor(usag$equipement_secu, 
                                 levels = c(1,2,3,4,9), 
                                 labels = c("Ceinture","Casque","Dispositif enfants","Equipement réfléchissant ","Autre"))
  
  usag$utilisation_equipement_secu <- factor(usag$utilisation_equipement_secu, 
                                             levels = c(1,2,3), 
                                             labels = c("Utilisation sécu","Non utilisation sécu","Non déterminable"))
  
  usag$locp <- factor(usag$locp, levels = c(1,2,3,4,5,6,7,8), labels = c("Sur chaussée A + 50 m du passage piéton","Sur chaussée A – 50 m du passage piéton","Sur passage piéton Sans signalisation lumineuse","Sur passage piéton Avec signalisation lumineuse","Sur trottoir","Sur accotement","Sur refuge ou BAU","Sur contre allée"))
  usag$actp <- factor(usag$actp, levels = c(1,2,3,4,5,6,9), labels = c("Se déplaçant Sens véhicule heurtant","Se déplaçant Sens inverse du véhicule","Traversant","Masqué","Jouant – courant","Avec animal","Autre"))
  usag$etatp <- factor(usag$etatp, levels = c(1,2,3), labels = c("Seul","Accompagné","En groupe"))
  
  # Lieux -------------------------------------------------------------------
  
  # Transformation des variables quantitatives en facteur
  lieux$Num_Acc <- factor(lieux$Num_Acc)
  lieux$catr <- factor(lieux$catr, 
                       levels =  c(1,2,3,4,5,6,9), 
                       labels = c("Autoroute","Route Nationale","Route Départementale","Voie Communale",
                                  "Hors réseau public","Parc de stationnement ouvert à la circulation publique","autre"))
  lieux$circ <- factor(lieux$circ, 
                       levels =  1:4, 
                       labels = c("A sens unique","Bidirectionnelle","A chaussées séparées","Avec voies d’affectation variable"))
  lieux$vosp <- factor(lieux$vosp, 
                       levels =  1:3, 
                       labels = c("Piste cyclable","Banque cyclable","Voie réservée"))
  lieux$prof <- factor(lieux$prof, 
                       levels =  1:4, 
                       labels = c("Plat","Pente","Sommet de côte","Bas de côte"))
  lieux$plan <- factor(lieux$plan, 
                       levels =  1:4, 
                       labels = c("Partie rectiligne","En courbe à gauche","En courbe à̀ droite","En « S »"))
  lieux$surf <- factor(lieux$surf, 
                       levels =  1:9, 
                       labels = c("Normale","Mouillée","Flaques","Inondée", "Enneigée", "Boue", "Verglacée", "Corps gras - huile", "Autre"))
  lieux$infra <- factor(lieux$infra, 
                        levels =  1:7, 
                        labels = c("Souterrain - tunnel","Pont - autopont","Bretelle d’échangeur ou de raccordement",
                                   "Voie ferrée", "Carrefour aménagé", "Zone piétonne", "Zone de péage"))
  lieux$situ <- factor(lieux$situ, 
                       levels =  1:5, 
                       labels = c("Sur chaussée","Sur bande d'arrêt d'urgence","Sur accotement",
                                  "Sur trottoir", "Sur piste cyclable"))
  
  # Clean env
  rm(equipement_secu, utilisation_equipement_secu)
  
  # Création du csv global, par individu ------------------------------------
  
  global_ind <- usag %>% 
    left_join(vehic) %>% 
    left_join(lieux) %>% 
    left_join(caract) %>%
    group_by(Num_Acc) %>% 
    mutate(nb_pers_impliquées = n()) %>%
    ungroup() %>% 
    mutate(int = if_else(int != "Hors intersection", true = "Intersection", false = "Hors intersection"), #pas de na
           zone = if_else(dep %in% c(75, 77, 78, 91, 92, 93, 94, 95), "IDF", "Province"),
           age = as.numeric(i) - an_nais,
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
             test = catr %in% c("Hors réseau public", 
                                "Parc de stationnement ouvert à la circulation publique", 
                                "autre"),
             yes = "Autre",
             no = as.character(catr)
           ),
           loc_pieton = case_when(
             stri_detect_fixed(locp, "Sur passage") ~ "Sur passage piéton",
             locp == "Sur chaussée A – 50 m du passage piéton" ~ "Sur passage piéton",
             locp %in% c("Sur chaussée A + 50 m du passage piéton", "Sur accotement", "Sur refuge ou BAU") ~ "Hors zone piétone",
             locp %in% c("Sur contre allée", "Sur trottoir") ~ "Zone piétone"
           ),
           action_pieton = case_when(
             stri_detect_fixed(actp, "Sens") ~ "se deplace",
             actp == "Avec animal" ~ "se deplace",
             actp == "Traversant" ~ "Traversant"
           )
           
    ) %>% 
    filter(gps == "Métropole",
           cat_vehic != "Autre véhicule") %>% 
    select(#-place, 
      -locp, -actp, -etatp, -an_nais, -num_veh, -equipement_secu, -senc, -occutc, -obs, -obsm, -manv,
      -voie, -v1, -v2, -pr, -pr1, -vosp, -lartpc, -larrout, -infra, -situ, -env1, -an, -mois, -jour, -hrmn, -com, 
      -adr, -gps, -circ, -catv, -atm, -col, -catr) 
  
  # write csv
  write.csv(global_ind, paste0("outputs/individus_", i, "_alldata.csv"), row.names = F)
  
}

# check csv
test2017 <- read.csv("outputs/individus_2017_alldata.csv")
test2018 <- read.csv("outputs/individus_2018_alldata.csv")
