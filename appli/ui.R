
library(shinythemes)
library(shiny)
library(shinyWidgets)
library(later)
library(htmltools)
library(colourpicker)
library(rAmCharts)
library(shinythemes)
library(leaflet)
library(dplyr) # dataset manipulation
library(tidylog) # provide feedback about dplyr operations
library(tidyverse)
library(ranger)
library(ggplot2)
library(purrr)
library(lattice)
library(htmlwidgets)
library(rpivotTable)
library(plotly)
library(RColorBrewer)
library(sf)

shinyUI(fluidPage(theme = shinytheme("cerulean"),
                  
                  navbarPage(
                    title = "Projet Datascience",
                    
                    tabPanel(title = "Stats Descriptives", icon(NULL), 

                               mainPanel(width=12,
                                         tabsetPanel(type = "tabs",
                                                     tabPanel("Présentation des données", br(),
                                                              em(h2("Base de données accidents corporels de la circulation",align = "center")),
                                                              hr(),
                                                              p("Pour chaque accident corporel (soit un accident survenu sur une voie ouverte à la circulation publique,
                                                                impliquant au moins un véhicule et ayant fait au moins une victime ayant nécessité des soins), 
                                                                des saisies d’information décrivant l’accident sont effectuées par l’unité des forces de l’ordre (police, gendarmerie, etc.)
                                                                qui est intervenue sur le lieu de l’accident. Ces saisies sont rassemblées dans une fiche intitulée bulletin d’analyse des accidents corporels.
                                                                L’ensemble de ces fiches constitue le fichier national des accidents corporels de la circulation dit Fichier BAAC 
                                                                administré par l’Observatoire national interministériel de la sécurité routière ONISR.
                                                                Les bases de données, extraites du fichier BAAC, répertorient l'intégralité des accidents corporels de la circulation 
                                                                intervenus durant une année précise en France métropolitaine. 
                                                                Cela comprend des informations de localisation de l’accident, telles que renseignées ainsi que des informations concernant
                                                                les caractéristiques de l’accident et son lieu, les véhicules impliqués et leurs victimes."),
                                                              hr(),
                                                              h4("Contenu des données",align = "center"),
                                                              p("Le Fichier BAAC et composé de 4 fichiers (Caractéristiques – Lieux – Véhicules – Usagers ). Vous trouvrer le decsiptif complet grace au lien ci-dessous :"),
                                                              br(),
                                                              a("https://www.data.gouv.fr/fr/datasets/r/8d4df329-bbbb-434c-9f1f-596d78ad529f"),
                                                              hr(),
                                                              h4("Aperçu des données",align = "center"), br(),tableOutput("view"),
                                                              hr(),
                                                              h4("Résumé des variables",align = "center"), br(),verbatimTextOutput("summary"),class = 'rightAlign'),
                                                     tabPanel("Manipulation des données", tags$head(tags$style(type = 'text/css','#myPivot{ overflow-x: scroll; }')),
                                                              rpivotTableOutput("myPivot", width = "100%", height = "700px")),
                                                     tabPanel("DashBoard",
                                                              fluidRow(column(width=12,br(),
                                                                              h2("Niveau de gravité des accidents au global"),align = "center",
                                                                              plotlyOutput('plot1'), hr()),
                                                                       column(width=12,
                                                                              h2("Niveau de gravité des accidents par indicateurs"),align = "center",br()),
                                                                       column(width=6,h3("Distribution au global"),align = "center",hr()),
                                                                       column(width=6,h3("Distribution par niveau de gravité"),align = "center",hr()),
                                                                       column(width=6,h5("Par sexe"),plotlyOutput('plot2')),
                                                                       column(width=6,plotlyOutput('plot3')),
                                                                       column(width=12),
                                                                       column(width=6,h5("Par age"),plotlyOutput('plot16')),
                                                                       column(width=6,plotlyOutput('plot17')),
                                                                       column(width=12),
                                                                       column(width=12,h5("Distribution de l'age"),plotlyOutput('plot20')),
                                                                       column(width=12),
                                                                       column(width=6,h5("Par type de vehicule"),plotlyOutput('plot4')),
                                                                       column(width=6,plotlyOutput('plot5')),
                                                                       column(width=12),
                                                                       column(width=6,h5("Par type d'usager"),plotlyOutput('plot14')),
                                                                       column(width=6,plotlyOutput('plot15')),
                                                                       column(width=12),
                                                                       column(width=6,h5("Par utilisation d'équipment de sécurité"),plotlyOutput('plot6')),
                                                                       column(width=6,plotlyOutput('plot7')),
                                                                       column(width=12),
                                                                       column(width=6, h5("Par type de voie"),plotlyOutput('plot8')),
                                                                       column(width=6,plotlyOutput('plot9')),
                                                                       column(width=12),
                                                                       column(width=6,h5("Par type d'agglomération"),plotlyOutput('plot10')),
                                                                       column(width=6,plotlyOutput('plot11')),
                                                                       column(width=12),
                                                                       column(width=6,h5("Par IDF/Province"),plotlyOutput('plot12')),
                                                                       column(width=6,plotlyOutput('plot13')),
                                                                       column(width=12)
                                                                       
                                                              )),
                                                     tabPanel("Carto",h3("Cartographie de la gravité des accidents par département",align = "center"),hr(),tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                                                              leafletOutput("map"),height="700px") 
                                                     
                                            )
                                         )
                             ),#Fin du TabPanelT1
                    
                    
                    tabPanel(title = "Appels",
                             wellPanel(
                               fluidRow(
                                 column(width=4,
                                        pickerInput(
                                          inputId = "dep",
                                          label = "Département", 
                                          choices = list(
                                            "Auvergne-Rhône-Alpes" = c("01 - Ain", "03 - Allier", "07 - Ardèche", "15 - Cantal",  "26 - Drôme", "38 - Isère", "42 - Loire", "43 - Haute-Loire", "63 - Puy-de-Dôme", "69 - Rhône", "73 - Savoie", "74 - Haute-Savoie"),
                                            "Bourgogne-Franche-Comté" = c("21 - Côte-d'Or","25 - Doubs","39 - Jura","58 - Nièvre","70 - Haute-Saône","71 - Saône-et-Loire","89 - Yonne","90 - Territoire de Belfort"),
                                            "Bretagne" = c("22 - Côtes d'Armor","29 - Finistère","35 - Ille-et-Vilaine","56 - Morbihan"),
                                            "Centre-Val de Loire" = c("18 - Cher","28 - Eure-et-Loir","36 - Indre","37 - Indre-et-Loire","41 - Loir-et-Cher","45 - Loiret"),
                                            "Grand Est" = c("08 - Ardennes", "10 - Aube","51 - Marne","52 - Haute-Marne","54 - Meurthe-et-Moselle","55 - Meuse","57 - Moselle","67 - Bas-Rhin","68 - Haut-Rhin","88 - Vosges"),
                                            "Hauts-de-France" = c("02 - Aisne","59 - Nord","60 - Oise","62 - Pas-de-Calais","80 - Somme"),
                                            "Ile-de-France" = c("75 - Paris","77 - Seine-et-Marne","78 - Yvelines","91 - Essonne","92 - Hauts-de-Seine","93 - Seine-St-Denis","94 - Val-de-Marne","95 - Val-D'Oise"),
                                            "Normandie" = c("14 - Calvados","27 - Eure","50 - Manche","61 - Orne","76 - Seine-Maritime"),
                                            "Nouvelle-Aquitaine" = c("16 - Charente","17 - Charente-Maritime","19 - Corrèze","23 - Creuse","24 - Dordogne","33 - Gironde","40 - Landes","47 - Lot-et-Garonne","64 - Pyrénées-Atlantiques","79 - Deux-Sèvres","86 - Vienne","87 - Haute-Vienne"),
                                            "Occitanie" = c("09 - Ariège","11 - Aude","12 - Aveyron","30 - Gard","31 - Haute-Garonne","32 - Gers","34 - Hérault","46 - Lot","48 - Lozère","65 - Hautes-Pyrénées","66 - Pyrénées-Orientales","81 - Tarn","82 - Tarn-et-Garonne"),
                                            "Pays de la Loire" = c("44 - Loire-Atlantique","49 - Maine-et-Loire","53 - Mayenne","72 - Sarthe","85 - Vendée"),
                                            "Provence-Alpes-Côte d'Azur" = c("04 - Alpes-de-Haute-Provence","05 - Hautes-Alpes","06 - Alpes-Maritimes","13 - Bouches-du-Rhône","83 - Var","84 - Vaucluse"))
                                        ),
                                 )),
                               
                               fluidRow(
                                 column(width=12,
                                        radioGroupButtons(inputId = "cat_route", label = "Lieu de l'accident",  individual = FALSE, justified = TRUE, size = "sm", choices = c("Autoroute", "Route Nationale", "Route Départementale", "Voie Communale", "Autre")),   #Retransformer route départ avec l'accent     
                                 )),
                               
                               
                               conditionalPanel(condition = "input.cat_route == 'Route Nationale' || input.cat_route == 'Route Départementale' || input.cat_route == 'Autre'",
                                                fluidRow(
                                                  column(width=4, radioGroupButtons(inputId = "agg", label = "Préciser le lieu", individual = FALSE, justified = TRUE, size = "sm", choices = c("Agglomération" = "Agglo", "Hors Agglomération" = "Hors Agglo")),),
                                                  column(width=4,""),             
                                                  column(width=4,""),
                                                ),
                               ),
                               
                               fluidRow(
                                 column(width=12, radioGroupButtons(inputId = "collision", label = "Type d'accident", individual = FALSE, justified = TRUE, size = "sm", choices = c("Aucune collision" = "Sans collision", 
                                                                                                                                                                                     "Deux véhicules, collision frontale" = "Deux véhicules - frontale & autre", 
                                                                                                                                                                                     "Deux véhicules, collision par le côté ou l'arrière" = "Deux véhicules", 
                                                                                                                                                                                     "Trois véhicules" = "Trois véhicules")),),
                               ),
                               
                               fluidRow(
                                 column(width=12, prettySwitch(inputId = "presence_PL", label = "Présence d'un poids lourd", status = "success", fill = TRUE),),
                               ),
                               
                             ), #fin du Well Panel (le fond gris)
                             
                             wellPanel(
                               fluidRow(
                                 column(width = 3, #1er quart de la page
                                        
                                        fluidRow(
                                          column(width=12, align = "center", prettySwitch(inputId = "vict1", label = "Victime n°1", fill = TRUE, value=TRUE),),
                                        ),
                                        
                                        conditionalPanel(condition = "input.vict1 == true",
                                                         
                                                         fluidRow(
                                                           column(width=12, align = "center", uiOutput("v1_prediction"),),
                                                         ),
                                                         
                                                         fluidRow(
                                                           column(width=12, radioGroupButtons(inputId = "v1_sexe", label = "Sexe", individual = FALSE, choices = c(`<i class="fas fa-male"></i> Homme` = "Masculin",`<i class="fas fa-female"></i> Femme` = "Féminin"), justified = TRUE),),
                                                         ),
                                                         
                                                         fluidRow(
                                                           column(width=6, numericInput(inputId = "v1_age", label= "Age", value = 0, min = 0, max = 110, step = NA, width = NULL),),
                                                         ),
                                                         
                                                         fluidRow(
                                                           column(width=12, radioGroupButtons(
                                                             inputId = "v1_cat_vehic",
                                                             justified = TRUE,
                                                             size = 'xs',
                                                             label = "Véhicule impliqué",
                                                             choices = c(`<i class="fas fa-car"></i>` = "Voiture",
                                                                         `<i class='fas fa-motorcycle'></i>>125` = "2 roues > 125 cm3", 
                                                                         `<i class='fas fa-motorcycle'></i><125` = "2 roues peu puissants",
                                                                         `<i class='fas fa-bicycle'></i>` = "Vélo",
                                                                         `<i class='fas fa-truck-moving'></i>` = "Poids lourds"
                                                             ),
                                                             individual = FALSE
                                                           )),
                                                         ),
                                                         
                                                         fluidRow(
                                                           column(width = 12, radioGroupButtons(inputId = "v1_catu", label = "Emplacement de la victime", size = "sm", justified = TRUE, choices = c("Conducteur", "Passager", "Piéton")),),
                                                         ),
                                                         
                                                         conditionalPanel(condition = "input.v1_catu == 'Passager'",
                                                                          fluidRow(
                                                                            column(width = 12, radioGroupButtons(inputId = "v1_place", label = "Préciser l'emplacement du passager", size = "sm", justified = TRUE, choices = c("Avant", "Arrière", "NSP" = "Non communiqué")),),
                                                                          ),
                                                         ),
                                                         
                                                         conditionalPanel(condition = "input.v1_catu == 'Piéton'",
                                                                          fluidRow(
                                                                            column(width = 12, radioGroupButtons(inputId = "v1_loc_pieton", label = "En zone piétonne", size = "sm", justified = TRUE, choices = c("Oui" = "Zone piétonne (dont passage piéton)", "Non" = "Hors zone piétonne", "NSP" = "Non communiqué")),),
                                                                          ),
                                                         ),
                                                         
                                                         conditionalPanel(condition = "input.v1_catu != 'Piéton'",
                                                                          fluidRow(
                                                                            column(width = 12, radioGroupButtons(inputId = "v1_equiq_secu", label = "Equipement de sécurité", size = "sm", justified = TRUE, choices = c("Oui" = "Utilisation sécu", "Non" = "Non utilisation sécu", "NSP" = "Non communiqué")),),
                                                                          ),
                                                         ),
                                                         
                                        ), # Fin du Conditionnal Panel sur la présence de la victime
                                 ), #Fin du 1er quart de page
                                 
                                 # Début du 2eme quart de page
                                 column(width = 3,  
                                        
                                        fluidRow(
                                          column(width=12, align = "center", prettySwitch(inputId = "vict2", label = "Victime n°2", fill = TRUE, value=FALSE),),
                                        ),
                                        
                                        
                                        conditionalPanel(condition = "input.vict2 == true",
                                                         
                                                         fluidRow(
                                                           column(width=12, align = "center", uiOutput("v2_prediction"),),
                                                         ),
                                                         
                                                         fluidRow(
                                                           column(width=12, radioGroupButtons(inputId = "v2_sexe", label = "Sexe", individual = FALSE, choices = c(`<i class="fas fa-male"></i> Homme` = "Masculin",`<i class="fas fa-female"></i> Femme` = "Féminin"), justified = TRUE),),
                                                         ),
                                                         
                                                         fluidRow(
                                                           column(width=6, numericInput(inputId = "v2_age", label= "Age", value = 0, min = 0, max = 110, step = NA, width = NULL),),
                                                         ),
                                                         
                                                         fluidRow(
                                                           column(width=12, radioGroupButtons(
                                                             inputId = "v2_cat_vehic",
                                                             justified = TRUE,
                                                             size = 'xs',
                                                             label = "Véhicule impliqué",
                                                             choices = c(`<i class="fas fa-car"></i>` = "Voiture",
                                                                         `<i class='fas fa-motorcycle'></i>>125` = "2 roues > 125 cm3", 
                                                                         `<i class='fas fa-motorcycle'></i><125` = "2 roues peu puissants",
                                                                         `<i class='fas fa-bicycle'></i>` = "Vélo",
                                                                         `<i class='fas fa-truck-moving'></i>` = "Poids lourds"
                                                             ),
                                                             individual = FALSE
                                                           )),
                                                         ),
                                                         
                                                         fluidRow(
                                                           column(width = 12, radioGroupButtons(inputId = "v2_catu", label = "Emplacement de la victime", size = "sm", justified = TRUE, choices = c("Conducteur", "Passager", "Piéton")),),
                                                         ),
                                                         
                                                         conditionalPanel(condition = "input.v2_catu == 'Passager'",
                                                                          fluidRow(
                                                                            column(width = 12, radioGroupButtons(inputId = "v2_place", label = "Préciser l'emplacement du passager", size = "sm", justified = TRUE, choices = c("Avant", "Arrière", "NSP" = "Non communiqué")),),
                                                                          ),
                                                         ),
                                                         
                                                         conditionalPanel(condition = "input.v2_catu == 'Piéton'",
                                                                          fluidRow(
                                                                            column(width = 12, radioGroupButtons(inputId = "v2_loc_pieton", label = "En zone piétonne", size = "sm", justified = TRUE, choices = c("Oui" = "Zone piétonne (dont passage piéton)", "Non" = "Hors zone piétonne", "NSP" = "Non communiqué")),),
                                                                          ),
                                                         ),
                                                         
                                                         conditionalPanel(condition = "input.v2_catu != 'Piéton'",
                                                                          fluidRow(
                                                                            column(width = 12, radioGroupButtons(inputId = "v2_equiq_secu", label = "Equipement de sécurité", size = "sm", justified = TRUE, choices = c("Oui" = "Utilisation sécu", "Non" = "Non utilisation sécu", "NSP" = "Non communiqué")),),
                                                                          ),
                                                         ),
                                                         
                                        ), # Fin du Conditionnal Panel sur la présence de la victime
                                 ), #Fin du 2e quart de page
                                 
                                 # Début du 3eme quart de page
                                 
                                 column(width = 3,  
                                        
                                        fluidRow(
                                          column(width=12, align = "center", prettySwitch(inputId = "vict3", label = "Victime n°3", fill = TRUE, value=FALSE),),
                                        ),
                                        
                                        
                                        conditionalPanel(condition = "input.vict3 == true",
                                                         
                                                         fluidRow(
                                                           column(width=12, align = "center", uiOutput("v3_prediction"),),
                                                         ),
                                                         
                                                         fluidRow(
                                                           column(width=12, radioGroupButtons(inputId = "v3_sexe", label = "Sexe", individual = FALSE, choices = c(`<i class="fas fa-male"></i> Homme` = "Masculin",`<i class="fas fa-female"></i> Femme` = "Féminin"), justified = TRUE),),
                                                         ),
                                                         
                                                         fluidRow(
                                                           column(width=6, numericInput(inputId = "v3_age", label= "Age", value = 0, min = 0, max = 110, step = NA, width = NULL),),
                                                         ),
                                                         
                                                         fluidRow(
                                                           column(width=12, radioGroupButtons(
                                                             inputId = "v3_cat_vehic",
                                                             justified = TRUE,
                                                             size = 'xs',
                                                             label = "Véhicule impliqué",
                                                             choices = c(`<i class="fas fa-car"></i>` = "Voiture",
                                                                         `<i class='fas fa-motorcycle'></i>>125` = "2 roues > 125 cm3", 
                                                                         `<i class='fas fa-motorcycle'></i><125` = "2 roues peu puissants",
                                                                         `<i class='fas fa-bicycle'></i>` = "Vélo",
                                                                         `<i class='fas fa-truck-moving'></i>` = "Poids lourds"
                                                             ),
                                                             individual = FALSE
                                                           )),
                                                         ),
                                                         
                                                         fluidRow(
                                                           column(width = 12, radioGroupButtons(inputId = "v3_catu", label = "Emplacement de la victime", size = "sm", justified = TRUE, choices = c("Conducteur", "Passager", "Piéton")),),
                                                         ),
                                                         
                                                         conditionalPanel(condition = "input.v3_catu == 'Passager'",
                                                                          fluidRow(
                                                                            column(width = 12, radioGroupButtons(inputId = "v3_place", label = "Préciser l'emplacement du passager", size = "sm", justified = TRUE, choices = c("Avant", "Arrière", "NSP" = "Non communiqué")),),
                                                                          ),
                                                         ),
                                                         
                                                         conditionalPanel(condition = "input.v3_catu == 'Piéton'",
                                                                          fluidRow(
                                                                            column(width = 12, radioGroupButtons(inputId = "v3_loc_pieton", label = "En zone piétonne", size = "sm", justified = TRUE, choices = c("Oui" = "Zone piétonne (dont passage piéton)", "Non" = "Hors zone piétonne", "NSP" = "Non communiqué")),),
                                                                          ),
                                                         ),
                                                         
                                                         conditionalPanel(condition = "input.v3_catu != 'Piéton'",
                                                                          fluidRow(
                                                                            column(width = 12, radioGroupButtons(inputId = "v3_equiq_secu", label = "Equipement de sécurité", size = "sm", justified = TRUE, choices = c("Oui" = "Utilisation sécu", "Non" = "Non utilisation sécu", "NSP" = "Non communiqué")),),
                                                                          ),
                                                         ),
                                                         
                                        ), # Fin du Conditionnal Panel sur la présence de la victime
                                 ), #Fin du 3e quart de page
                                 
                                 # Début du 4eme quart de page
                                 column(width = 3,  
                                        
                                        fluidRow(
                                          column(width=12, align = "center", prettySwitch(inputId = "vict4", label = "Victime n°4", fill = TRUE, value=FALSE),),
                                        ),
                                        
                                        
                                        conditionalPanel(condition = "input.vict4 == true",
                                                         
                                                         fluidRow(
                                                           column(width=12, align = "center", uiOutput("v4_prediction"),),
                                                         ),
                                                         
                                                         fluidRow(
                                                           column(width=12, radioGroupButtons(inputId = "v4_sexe", label = "Sexe", individual = FALSE, choices = c(`<i class="fas fa-male"></i> Homme` = "Masculin",`<i class="fas fa-female"></i> Femme` = "Féminin"), justified = TRUE),),
                                                         ),
                                                         
                                                         fluidRow(
                                                           column(width=6, numericInput(inputId = "v4_age", label= "Age", value = 0, min = 0, max = 110, step = NA, width = NULL),),
                                                         ),
                                                         
                                                         fluidRow(
                                                           column(width=12, radioGroupButtons(
                                                             inputId = "v4_cat_vehic",
                                                             justified = TRUE,
                                                             size = 'xs',
                                                             label = "Véhicule impliqué",
                                                             choices = c(`<i class="fas fa-car"></i>` = "Voiture",
                                                                         `<i class='fas fa-motorcycle'></i>>125` = "2 roues > 125 cm3", 
                                                                         `<i class='fas fa-motorcycle'></i><125` = "2 roues peu puissants",
                                                                         `<i class='fas fa-bicycle'></i>` = "Vélo",
                                                                         `<i class='fas fa-truck-moving'></i>` = "Poids lourds"
                                                             ),
                                                             individual = FALSE
                                                           )),
                                                         ),
                                                         
                                                         fluidRow(
                                                           column(width = 12, radioGroupButtons(inputId = "v4_catu", label = "Emplacement de la victime", size = "sm", justified = TRUE, choices = c("Conducteur", "Passager", "Piéton")),),
                                                         ),
                                                         
                                                         conditionalPanel(condition = "input.v4_catu == 'Passager'",
                                                                          fluidRow(
                                                                            column(width = 12, radioGroupButtons(inputId = "v4_place", label = "Préciser l'emplacement du passager", size = "sm", justified = TRUE, choices = c("Avant", "Arrière", "NSP" = "Non communiqué")),),
                                                                          ),
                                                         ),
                                                         
                                                         conditionalPanel(condition = "input.v4_catu == 'Piéton'",
                                                                          fluidRow(
                                                                            column(width = 12, radioGroupButtons(inputId = "v4_loc_pieton", label = "En zone piétonne", size = "sm", justified = TRUE, choices = c("Oui" = "Zone piétonne (dont passage piéton)", "Non" = "Hors zone piétonne", "NSP" = "Non communiqué")),),
                                                                          ),
                                                         ),
                                                         
                                                         conditionalPanel(condition = "input.v4_catu != 'Piéton'",
                                                                          fluidRow(
                                                                            column(width = 12, radioGroupButtons(inputId = "v4_equiq_secu", label = "Equipement de sécurité", size = "sm", justified = TRUE, choices = c("Oui" = "Utilisation sécu", "Non" = "Non utilisation sécu", "NSP" = "Non communiqué")),),
                                                                          ),
                                                         ),
                                                         
                                        ), # Fin du Conditionnal Panel sur la présence de la victime
                                 ), #Fin du 4e quart de page
                                 
                               ),
                               actionButton("bouton_calcul", label= "Calculer la gravité", class = "btn-primary", width='100%'),
                               
                             ), #Fin du deuxieme WellPanel
                    )
                    # tabPanel(title = "Onglet 3",  
                    #          column(width = 12,align = "center",
                    #                 tableOutput(outputId = "table"),
                    #                 #verbatimTextOutput("strfile"),
                    #                 
                    #                 textOutput("Pred"),
                    #                 progressBar(
                    #                   id = "pb2",
                    #                   value = 0,
                    #                   total = 100,
                    #                   title = "",
                    #                   display_pct = TRUE
                    #                 ),
                    #                 #htmlOutput("text3"),
                    #                 #uiOutput("val2")
                    #                 #),
                    #                 #verbatimTextOutput("pred"),
                    #                 #uiOutput(outputId = "fluidRow_ui")
                    #          )
                    #          
                    # )
                    )
                  ))