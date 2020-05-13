# Librairires -------------------------------------------------------------

install.packages("corrplot") # corrélation
install.packages("ggplot2") # data visualisation
install.packages("forcats") # factor manipulation
install.packages("dplyr") # dataset manipulation
install.packages("kableExtra") # create nice html tables
install.packages("stringi") # string manipulation
install.packages("patchwork") # combine plots


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
    base_size = 15
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

# Importation des fichiers --------------------------------------------------------

annee <- 2017


caract <- read.csv(file = paste0("data/caracteristiques-",annee,".csv"), header = TRUE, sep = ",")
vehic <- read.csv(file = paste0("data/vehicules-",annee,".csv"), header = TRUE, sep = ",")
usag <- read.csv(file = paste0("data/usagers-",annee,".csv"), header = TRUE, sep = ",")
lieux <- read.csv(file = paste0("data/lieux-",annee,".csv"), header = TRUE, sep = ",")

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
                                           labels = c("Oui","Non","Non déterminable"))

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

# Réattribution dans les bons objets  -------------------------------------------------------------------
assign(paste0("caract",annee),caract)
assign(paste0("vehic",annee),vehic)
assign(paste0("usag",annee),usag)
assign(paste0("lieux",annee),lieux)

remove(list = c("caract","vehic","usag","lieux"))

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


####Export

library(writexl) 

write_xlsx(caract2017, "data/spss/caract2017.xlsx")
write_xlsx(lieux2017, "data/spss/lieux2017.xlsx")
write_xlsx(usag2017, "data/spss/usag2017.xlsx")
write_xlsx(vehic2017, "data/spss/vehic2017.xlsx")


###Fusion


# bdd_complete_test<-merge(usag2017,caract2017,by="Num_Acc",all.x = TRUE, all.y = TRUE)
# bdd_complete_test<-merge(bdd_complete_test,vehic2017,by="Num_Acc",all.x = TRUE, all.y = TRUE)
# 
# 
# bdd_complete_test <- (usag2017 %>% full_join(caract2017, by ="Num_Acc") 
#                                %>% inner_join(vehic2017, by ="Num_Acc")
#                                %>% inner_join(usag2017, by ="Num_Acc"))
# 
# 
# bdd_complete_test <- bdd_complete_test %>% left_join(vehic2017, by ="Num_Acc")


# Typologies d'accidents - on ne garde que le blessé le plus grave par accident
global_acc <- usag2017 %>% 
  inner_join(caract2017) %>% 
  mutate(grav = factor(grav, levels = c("Tué", "Blessé hospitalisé", "Blessé léger", "Indemne")),
         int = if_else(int != "Hors intersection", "Intersection", "Hors intersection"),
         zone = if_else(dep %in% c(75, 77, 78, 91, 92, 93, 94, 95), "IDF", "Province")
  ) %>% 
  arrange(Num_Acc, grav) %>% 
  group_by(Num_Acc) %>% 
  mutate(rank = seq(1:n())) %>% 
  filter(rank == 1) %>% 
  inner_join(vehic2017) %>% 
  inner_join(lieux2017) %>% 
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



#----------------nbr et freq
table(global_acc$sexe)
prop.table(table(global_acc$sexe))

#---------------pie ensemble
colors<-c("blue","red","darkred")
pie(table(global_acc$sexe),col=colors,main="Répartition sexe",
    labels=round(table(global_acc$sexe)/nrow(global_acc)*100,digits=3))
legend("bottomleft", xpd = TRUE, legend = unique(global_acc$sexe),fill=colors)


# ggplot(global_acc) +
#   aes(x = sexe,stat="count") +
#   geom_histogram(fill ="orange", colour = "black", binwidth = 2) +
#   ggtitle("Répartition sexe") +
#   xlab("sexe") +
#   ylab("Effectifs")

#---------------Diagramme ensemble
ggplot(global_acc, aes(x = sexe , fill =sexe )) + 
  geom_bar(aes(y = ..count../sum(..count..))) +
  scale_y_continuous(labels=scales::percent) +
  ggtitle("Répartition") +
  # xlab(" ") + 
  ylab("Proportion")

# ggplot(global_acc, aes(x= grav,  group=sexe)) + 
#   geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
#   geom_text(aes( label = scales::percent(..prop..),
#                  y= ..prop.. ), stat= "count", vjust = -2) +
#   labs(y = "Proportion", fill="Gravité") +
#   facet_grid(~sexe) +
#   scale_y_continuous(labels = scales::percent)

#---------------Diagramme par gravité
ggplot(global_acc, aes(x= sexe,  group=grav)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = 1) +
  labs(y = "Proportion", fill="Sexe") +
  facet_grid(~grav) +
  scale_y_continuous(labels = scales::percent)

# verif<- usag2017 
# 
sapply(global_acc, function(x) sum(!duplicated(x)))



# # dplyr
# caract2017 %>% summarise_all(funs(n_distinct))
# # en combinant les deux :
# sapply(caract2017, n_distinct)

