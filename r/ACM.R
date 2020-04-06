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

rm(pietons)

# acm
acm <- MCA(caract_pieton, quali.sup = 6, graph = F)
plot(acm, 
     quali.sup = 6, 
     invisible = 'ind')

# Gravité de l'accident expliqué par ses caractéristiques

# Préparation de la donnée
caract_grav <- usag %>% 
  inner_join(caract) %>% 
  mutate(grav = factor(grav, levels = c("Tué", "Blessé hospitalisé", "Blessé léger", "Indemne")),
         lum = factor(lum, levels = c("Crépuscule ou aube", "Plein jour", "Nuit avec éclairage public allumé", "Nuit avec éclairage public non allumé", "Nuit sans éclairage public")),
         int = if_else(int != "Hors intersection", "Intersection", "Hors intersection")) %>% 
  arrange(Num_Acc, grav) %>% 
  group_by(Num_Acc) %>% 
  mutate(rank = seq(1:n())) %>% 
  filter(rank == 1) %>% 
  mutate(zone = if_else(dep %in% c(75, 77, 78, 91, 92, 93, 94, 95), "IDF", "Province")) %>% 
  ungroup() %>% 
  select(lum, agg, int, atm, zone, grav)

# acm
acm <- MCA(caract_grav, quali.sup = 6, graph = F)
plot(acm, 
     quali.sup = 6, 
     invisible = 'ind')

# Lieu de l'accident ------------------------------------------------------


# Usagers -----------------------------------------------------------------


# Véhicules ---------------------------------------------------------------


# Global ------------------------------------------------------------------



