#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(purrr)
library(dplyr)
library(shinydashboard)
library(lattice)
library(shinythemes)
#library(devtools)
library(htmlwidgets)
library(rpivotTable)
# library(semantic.dashboard)
library(plotly)
# library(DT)
library(sf)
library(leaflet)


# Define UI for dataset viewer app ----
ui <- fluidPage(theme = shinytheme("cerulean"),

  # App title ----
  titlePanel("Stats Déscriptives"),

  # Sidebar layout with a input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(width=2,
    h4("Selection des données:"),
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "",
                  choices = c("2017", "2018"))),

    # Main panel for displaying outputs ----
    mainPanel(width=8,
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
                                   h4("Aprerçu des données",align = "center"), br(),tableOutput("view"),
                                   hr(),
                                   h4("Résumé des variables",align = "center"), br(),verbatimTextOutput("summary"),class = 'rightAlign'),
                          tabPanel("Manipulation des données", tags$head(tags$style(type = 'text/css','#myPivot{ overflow-x: scroll; }')),
                                    rpivotTableOutput("myPivot", width = "100%", height = "700px")),
                          tabPanel("DashBoard",
                                   fluidRow(column(width=12,br(),
                                                   h3("Niveau de gravité des accidents au global"),align = "center",
                                                   plotlyOutput('plot1'), hr()),
                                            h3("Niveau de gravité des accidents par indicateurs"),align = "center",br(),
                                            column(width=6,
                                                   h5("Par sexe"),
                                                   plotlyOutput('plot2')),
                                            column(width=6,
                                                   h5("Par type d'usager"),
                                                   plotlyOutput('plot3')),
                                            br(),
                                            column(width=6,
                                                   h5("Par type de vehicule"),
                                                   plotlyOutput('plot4')),
                                            column(width=6,
                                                   h5("Par équipement de sécurité"),
                                                   plotlyOutput('plot5')),
                                            h3("Niveau de gravité des accidents par zone"),align = "center",br(),
                                            column(width=6,
                                                   h5("Par type d'agglo"),
                                                   plotlyOutput('plot6')),
                                            column(width=6,
                                                   h5("Par IDF/Province"),
                                                   plotlyOutput('plot7')),
                                            column(width=12,
                                                   "Distribution de l'age",
                                                   plotlyOutput('plot10'))

                                            )),
                           tabPanel("Carto",h3("Cartographie de la gravité des accidents par département",align = "center"),hr(),tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                                    leafletOutput("map"),height="700px") 

      )
    )
  )
)

