
#############################
################Packages----#
#############################

install.packages("stringr")
install.packages("knitr")
install.packages("viridis")
install.packages("numform")
install.packages("RColorBrewer")


library(dplyr)
library(tidyr)
library(forcats)
library(scales)
library(sf)
library(leaflet)
library(stringr)
library(knitr)
library(viridis)
library(numform)
library(RColorBrewer)


#####################################################################
#########################Récupérer les données géographiques DPT----#
#####################################################################


france_dep <- st_read(dsn = "data/departements-20180101.shp", layer = "departements-20180101", quiet = TRUE)

plot(france_dep)
head(france_dep)
cat("\n\n")

#Ajouter les centres de chaque département
france_dep <- france_dep %>% 
  mutate(centroid_lng = st_coordinates(st_centroid(geometry))[,1],
         centroid_lat = st_coordinates(st_centroid(geometry))[,2])

#Les données de département comportent une spécificité concernant le département du Rhône : 
#2 observations sont présentes, “Rhône” et “Métropole de Lyon”.

france_dep %>% 
  filter(str_detect(string = code_insee, pattern = "69")) %>% 
  st_set_geometry(NULL) %>% 
  kable() %>% 
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F, position = "left")

# extraire la géométrie du département "Rhône"
rhone_geom <- france_dep %>% 
  filter(str_detect(string = code_insee, pattern = "69")) %>% 
  st_union() %>% 
  st_geometry()

# ré-affecer cette géométrie à la ligne du département, et supprimer la ligne de la métropole de Lyon
france_dep$geometry[france_dep$code_insee == "69D"] <- rhone_geom

france_dep <- france_dep %>% 
  filter(code_insee != "69M") %>% 
  mutate(code_insee = fct_recode(code_insee, "69" = "69D")) %>% 
  mutate_if(is.factor, .funs = function(x) fct_drop(x))

france_dep %>% 
  filter(str_detect(string = code_insee, pattern = "69")) %>% 
  st_set_geometry(NULL) %>% 
  kable() %>% 
  kableExtra::kable_styling(bootstrap_options = "striped", full_width = F, position = "left")



#############################
#Préparation des données----#
#############################

baac2017 <- read.csv("outputs/individus_2017_alldata.csv")
baac2018 <- read.csv("outputs/individus_2018_alldata.csv")


#2017

carto2017 <- baac2017 %>%
group_by(dep,grav) %>% 
summarise(n= n()) %>%
ungroup() %>%
spread(grav, n, fill=0) %>%
mutate(tot=rowSums(carto2017[,2:5]),
       sgrav=rowSums(carto2017[,c(2,5)]),
       sngrav=rowSums(carto2017[,c(3,4)]),
       pctsgrav=sgrav/tot,
       pctsngrav=sngrav/tot,
       ratiof=pctsngrav/pctsgrav,
       ratio_grp = case_when(ratiof < 1.27                ~ "Très elevée",
                             between(ratiof, 1.27, 2.39)  ~ "Elevée",
                             between(ratiof, 2.39, 3.94)  ~ "Moyennement elevée",
                             TRUE                         ~ "Peu elevée"),
       ratio_grp = fct_relevel(ratio_grp, "Très elevée", "Elevée", "Moyennement elevée", "Peu elevée"),
       dep=sprintf("%02d", as.numeric(dep)),
       dep=factor(dep)) %>%
filter(dep!="97")

#2018

carto2018 <- baac2018 %>%
  group_by(dep,grav) %>% 
  summarise(n= n()) %>%
  ungroup() %>%
  spread(grav, n, fill=0) %>%
  mutate(tot=rowSums(carto2018[,2:5]),
         sgrav=rowSums(carto2018[,c(2,5)]),
         sngrav=rowSums(carto2018[,c(3,4)]),
         pctsgrav=sgrav/tot,
         pctsngrav=sngrav/tot,
         ratiof=pctsngrav/pctsgrav,
         ratio_grp = case_when(ratiof < 1.27                ~ "Très elevée",
                               between(ratiof, 1.27, 2.39)  ~ "Elevée",
                               between(ratiof, 2.39, 3.94)  ~ "Moyennement elevée",
                               TRUE                         ~ "Peu elevée"),
         ratio_grp = fct_relevel(ratio_grp, "Très elevée", "Elevée", "Moyennement elevée", "Peu elevée"),
         dep=sprintf("%02d", as.numeric(dep)),
         dep=factor(dep)) %>%
  filter(dep!="97")


#############################
###################Carto----#
#############################


# créer une palette de couleur correspondant aux groupes
ratio_grp_col <- colorFactor(palette = brewer.pal(4,'RdYlGn'), domain = unique(test2017_bis$ratio_grp), reverse = F) 

# Mise en forme du popup
labels <- sprintf(
  "<strong>",france_dep$nom,"<strong>") %>% 
  lapply(htmltools::HTML)

# carte
france_dep %>% 
  filter(nom != c("Mayotte","La Réunion","Guyane","Guadeloupe","Martinique","Corse-du-Sud","Haute-Corse")) %>%   # pas de données pour Mayotte
  st_simplify(preserveTopology = TRUE, dTolerance = 0.01) %>% 
  left_join(carto2017, by = c("code_insee" = "dep"))%>% 
  leaflet() %>% 
  addTiles() %>% 
  setView(lng = 2.866, lat = 46.56, zoom = 6) %>% 
  addPolygons(weight = 0.1, 
              fillColor = ~ ratio_grp_col(ratio_grp),
              col = "grey80",
              opacity = 0.7,
              fillOpacity = 0.6,
              highlightOptions = highlightOptions(weight = 2, bringToFront = TRUE),
              label = ~nom,
              popup = ~ paste("<b>", nom, "</b>", "<br>",
                              "<hr>","</hr>",
                              sgrav, " Accidents graves", "<br>",
                              sngrav, " Accidents légers", "<br>",
                              tot, " Accidents au total", "<br>"),
              labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "10px",
              direction = "auto")) %>%
  addLegend(position = "topright",
            pal = ratio_grp_col,
            values = ~ ratio_grp,
            title = "Niveau de gravité des accidents",
            opacity = 0.6
  )

france_dep


#############################
###################Save----#
#############################

save( list="baac2017", "baac2018", "carto2017", "carto2018", "france_dep","labels","ratio_grp_col",
      file="app/carto.RData")
