# Librairires -------------------------------------------------------------
library("corrplot") # corrélation
library("ggplot2") # data visualisation
library("forcats") # factor manipulation
library("dplyr") # dataset manipulation
library("kableExtra") # create nice html tables
library("stringi") # string manipulation
library("patchwork") # combine plots

# Thème ggplot ------------------------------------------------------------

# set default ggplot theme
theme_set(
  theme_light(
    base_size = 20
  ) +
    theme(
      text = element_text(family = "Gibson", colour = "gray10"),
      panel.border = element_blank(),
      axis.line = element_line(colour = "gray50", size = .5),
      axis.ticks = element_blank(),
      #axis.title.y = element_text(angle = 0),
      strip.background = element_rect(colour = "gray50", fill = "transparent", size = .7),
      strip.text.x = element_text(colour = "gray10"),
      legend.key.size = unit(2, "cm")
    )
)

# set default scales
discrete_colors <- c("#3D6CE8", "#EA619D", "#EACF61", "#86C7EC", "#86ECCB")
scale_colour_continuous <- function(...) scale_colour_viridis_c(..., option = "magma")
scale_colour_discrete <- function(...) scale_colour_manual(..., values = discrete_colors)
scale_fill_continuous <- function(...) scale_fill_viridis_c(..., option = "magma")
scale_fill_discrete <- function(...) scale_fill_manual(..., values = discrete_colors)


# Caractéristiques --------------------------------------------------------

# Import du fichier
caract <- read.csv(file = "../data/caracteristiques-2018.csv", header = TRUE, sep = ",")

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

# Import du fichier
vehic <- read.csv(file = "../data/vehicules-2018.csv", header = TRUE, sep = ",")

# Transformation des variables quantitatives en facteur
vehic$Num_Acc <- factor(vehic$Num_Acc)

vehic$senc <- factor(vehic$senc, levels = c(1,2), labels = c("Croissant","Décroissant"))

vehic$catv <- factor(vehic$catv, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,30,31,32,33,34,35,36,37,38,39,40,99), labels = c("Bicyclette", "Cyclomoteur < 50cm3", "Voiturette","04","05","06","VL seul", "08", "09", "VU seul 1,5T <= PTAC <= 3,5T avec ou sans remorque","11", "12", "PL seul 3,5T <PTCA <= 7,5T","PL seul > 7,5T"," PL > 3,5T + remorque", "Tracteur routier seul","Tracteur routier + semi-remorque","18","19", "Engin spécial","Tracteur agricole","Scooter < 50 cm3","Motocyclette > 50 cm3 et <= 125 cm3","Scooter > 50 cm3 et <= 125 cm3","Motocyclette > 125 cm3","Scooter > 125 cm3","Quad léger <= 50 cm3", "Quad lourd > 50 cm3", "Autobus", "Autocar", "Train", "Tramway", "Autre véhicule"))

vehic$obs <- factor(vehic$obs, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), labels = c("Véhicule en stationnement","Arbre","Glissière métallique","Glissière béton","Autre glissière","Bâtiment, mur, pile de pont","Support de signalisation verticale ou poste d’appel d’urgence","Poteau","Mobilier urbain","Parapet","Ilot, refuge, borne haute","Bordure de trottoir","Fossé, talus, paroi rocheuse","Autre obstacle fixe sur chaussée","Autre obstacle fixe sur trottoir ou accotement","Sortie de chaussée sans obstacle"))

vehic$obsm <- factor(vehic$obsm, levels = c(1,2,4,5,6,9), labels = c("Piéton", "Véhicule", "Véhicule sur rail", "Animal domestique", "Animal sauvage", "Autre"))

vehic$choc <- factor(vehic$choc, levels = c(1,2,3,4,5,6,7,8,9), labels = c("Avant","Avant droit","Avant gauche","Arrière","Arrière droit","Arrière gauche","Côté droit","Côté gauche","Chocs multiples (tonneaux)"))

vehic$manv <- factor(vehic$manv, levels =  c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24), labels = c("Sans changement de direction","Même sens, même file","Entre 2 files","En marche arrière","A contresens","En franchissant le terre-plein central","Dans le couloir bus, dans le même sens","Dans le couloir bus, dans le sens inverse","En s’insérant","En faisant demi-tour sur la chaussée","Changeant de file A gauche","Changeant de file A droite","Déporté A gauche","Déporté A droite","Tournant A gauche","Tournant A droite","Dépassant A gauche","Dépassant A droite","Traversant la chaussée","Manoeuvre de stationnement","Manoeuvre d’évitement","Ouverture de porte","Arrêté (hors stationnement)","En stationnement (avec occupants)"))

# Usagers -----------------------------------------------------------------

# Import du fichier
usag <- read.csv(file = "../data/usagers-2018.csv", header = TRUE, sep = ",")

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
                               labels = c("Ceinture","Casque","Dispositif enfants","Equipement réfléchissant ","Autre"))

usag$utilisation_equipement_secu <- factor(usag$utilisation_equipement_secu, 
                                           levels = c(1,2,3), 
                                           labels = c("Oui","Non","Non déterminable"))

usag$locp <- factor(usag$locp, levels = c(1,2,3,4,5,6,7,8), labels = c("Sur chaussée A + 50 m du passage piéton","Sur chaussée A – 50 m du passage piéton","Sur passage piéton Sans signalisation lumineuse","Sur passage piéton Avec signalisation lumineuse","Sur trottoir","Sur accotement","Sur refuge ou BAU","Sur contre allée"))

usag$actp <- factor(usag$actp, levels = c(1,2,3,4,5,6,9), labels = c("Se déplaçant Sens véhicule heurtant","Se déplaçant Sens inverse du véhicule","Traversant","Masqué","Jouant – courant","Avec animal","Autre"))

usag$etatp <- factor(usag$etatp, levels = c(1,2,3), labels = c("Seul","Accompagné","En groupe"))

# Lieux -------------------------------------------------------------------

# Import du fichier
lieux <- read.csv(file = "../data/lieux-2018.csv", header = TRUE, sep = ",")

# Transformation des variables quantitatives en facteur
lieux$Num_Acc <- factor(lieux$Num_Acc)

lieux$catr <- factor(lieux$catr, 
                     levels =  c(1,2,3,4,5,6,9), 
                     labels = c("Autoroute","Route Nationale","Route Départementale","Voie Communale",
                                "Hors réseau public","Parc de stationnement ouvert à la circulation publique","autre"))

lieux$circ <- factor(lieux$circ, 
                     levels =  1:4, 
                     labels = c("A sens unique","Bidirectionnelle","A chaussées séparées","Avec voies d’affectation variable"))

lieux$vosp <- factor(lieux$vosp, 
                     levels =  1:3, 
                     labels = c("Piste cyclable","Banque cyclable","Voie réservée"))

lieux$prof <- factor(lieux$prof, 
                     levels =  1:4, 
                     labels = c("Plat","Pente","Sommet de côte","Bas de côte"))

lieux$plan <- factor(lieux$plan, 
                     levels =  1:4, 
                     labels = c("Partie rectiligne","En courbe à gauche","En courbe à̀ droite","En « S »"))


lieux$surf <- factor(lieux$surf, 
                     levels =  1:9, 
                     labels = c("Normale","Mouillée","Flaques","Inondée", "Enneigée", "Boue", "Verglacée", "Corps gras - huile", "Autre"))

lieux$infra <- factor(lieux$infra, 
                      levels =  1:7, 
                      labels = c("Souterrain - tunnel","Pont - autopont","Bretelle d’échangeur ou de raccordement",
                                 "Voie ferrée", "Carrefour aménagé", "Zone piétonne", "Zone de péage"))

lieux$situ <- factor(lieux$situ, 
                     levels =  1:5, 
                     labels = c("Sur chaussée","Sur bande d'arrêt d'urgence","Sur accotement",
                                "Sur trottoir", "Sur piste cyclable"))

# Fonctions ---------------------------------------------------------------

# Valeurs manquantes
valeurs_manquantes <- function(data) {
  vm <- sapply(X = data,
               function(x)
                 sum(is.na(x)))
  
  vm <- data.frame(var = names(vm), nb_na = vm)
  
  ggplot(vm, mapping = aes(x = fct_reorder(var, nb_na, .desc = T), y = nb_na)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste(round(
      nb_na / nrow(data) * 100, 1
    ), "%")), vjust = -0.5, size = 2) +
    labs(x = "",
         y = "Nombre de valeurs manquantes") +
    theme(axis.text.x = element_text(angle = 70, hjust = 1))
}

# Distribution variables qualitatives

distr_quali <- function(data, variable, titre = "Titre du graphique", sort = TRUE) {
  
  variable <- enquo(variable)
  
  if (sort == TRUE) {
    plot <- data %>%
      # mutate(!!variable = fct_explicit_na(!!variable)) %>%
      group_by(!!variable) %>%
      summarise(n = n()) %>%
      ggplot(aes(x = fct_reorder(!!variable, n, .desc = T), y = n)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste(round(
        n / nrow(data) * 100, 1
      ), "%")), vjust = -0.5, size = 2) +
      labs(title = titre,
           x = "",
           y = "Nombre d'enregistrements") +
      theme(text = element_text(size = 12),
            axis.text.x = element_text(angle = 70, hjust = 1))
  }
  
  if (sort == FALSE) {
    plot <- data %>%
      # mutate(!!variable = fct_explicit_na(!!variable)) %>%
      group_by(!!variable) %>%
      summarise(n = n()) %>%
      ggplot(aes(x = !!variable, y = n)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste(round(
        n / nrow(data) * 100, 1
      ), "%")), vjust = -0.5, size = 2) +
      labs(title = titre,
           x = "",
           y = "Nombre d'enregistrements") +
      theme(text = element_text(size = 12),
            axis.text.x = element_text(angle = 70, hjust = 1))
    
  }
  return(plot)
}
