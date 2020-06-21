#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(colourpicker)
library(rAmCharts)
library(tidyverse)
library(caret)
library(mlr)


# # Importation des modèles
# modele_conduc <- readRDS(file = "rf_conducteurs.RDS")
# modele_passager <- readRDS(file = "rf_passagers.RDS")
# modele_pieton <- readRDS(file = "rf_pietons.RDS")
# load(file="data_baac.RData")
# 
# # Importation des open data supplémentaires
# alcool_et_accidents <- read.csv(file = 'alcool_et_accidents.csv', sep = ";", dec = ",")
# alcool_et_accidents$dep <- as.character(alcool_et_accidents$dep)


# Debut du Shiny Server
shinyServer(function(input, output, session) {
  
  
  #  Return the requested dataset ----
  # datasetInput <- reactive({
  #   switch(input$dataset,
  #          "2017" = baac2017,
  #          "2018" = baac2018
  #   )
  # })
  
  # summary du dataset ----
  output$summary <- renderPrint({
    dataset <- baac2018
    summary(dataset)
  })
  
  # vue des 10 premiéres obs ----
  output$view <- renderTable({
    head(baac2018, n = 10)
  })
  

  ############################################################################## Croiseur----

  output$myPivot <- renderRpivotTable(rpivotTable(data = baac2018,rows = "sexe", cols = "grav", aggregatorName="Count as Fraction of Columns"))
  
  ############################################################################## Dashboard---- 
  
  #Création de la palétte de couleur pour la gravité des accidents
  
  colors_gravite2 <- c("#29CF56","#A5F4A5","#FCA74F","#E03C31") 
  
  data_plot1 <- baac2018 %>%
    group_by(grav) %>%
    summarise(n = n()) %>%
    mutate(Pourcentages = round(n / sum(n)*100,1))
  
  ################ Au Global  
  
  data_plot1B<-data.frame(data_plot1,colors_gravite2)
  
  output$plot1 <- renderPlotly(
    plot1 <-plot_ly(data_plot1B,
                    labels = ~grav,
                    values = ~Pourcentages, 
                    type = 'pie',
                    textposition = 'inside',
                    textinfo = 'label+percent',
                    text = ~paste(grav),
                    insidetextfont = list(color = '#FFFFFF'),
                    hoverinfo = "text",
                    hovertext = paste("Volume :", data_plot2$n),
                    marker = list(colors = ~colors_gravite2, line = list(color = '#FFFFFF', width = 2))) %>%
      layout(title = '',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             showlegend = FALSE
      ))
  
  
################ Par sexe
  
##Volume
  
  dfsexe <- baac2018
  dfsexe <- dfsexe %>% group_by(sexe)
  dfsexe <- dfsexe %>% summarize(count = n())
  
  
  output$plot2 <- renderPlotly(
    plot2 <- plot_ly(dfsexe,
                        labels = ~sexe,
                        values = ~count,
                        text = ~paste(sexe),
                        hoverinfo = "text",
                        hovertext = paste("Volume :", dfsexe$count)
                        ) %>% 
                        add_pie(hole = 0.6) %>% 
                        layout(title = "",  showlegend = F,
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))

##Stack  
      
  
  data_plot2 <- baac2018 %>%
    group_by(sexe, grav) %>%
    summarise(n = n()) %>%
    mutate(freq = round(n / sum(n)*100)) %>%
    select(-n) %>%
    spread(grav, freq) %>%
    select(sexe,
           Tue = "Tué",
           Grave = "Blessé hospitalisé",
           Leger = "Blessé léger",
           Indemne)

  data_plot2B<-as.data.frame(data_plot2)


  output$plot3 <- renderPlotly(
    plot3 <- plot_ly(data_plot2B, x = ~sexe, y = ~Indemne, type = 'bar', name = 'Indemne',hoverinfo='text',text=paste("Indemne", data_plot2B$Indemne,"%" ),marker = list(color ="#407940")) %>% 
    add_trace(y = ~Leger, name = 'Blessé léger',hoverinfo='text',text=paste("Blessé léger", data_plot2B$Leger,"%" ), marker = list(color ="#90EE90")) %>% 
    add_trace(y = ~Grave, name = 'Blessé hospitalisé',hoverinfo='text',text=paste("Blessé hospitalisé", data_plot2B$Grave,"%" ), marker = list(color ="#FCA74F")) %>% 
    add_trace(y = ~Tue, name = 'Tué',hoverinfo='text',text=paste("Tué", data_plot2B$Tue,"%" ), marker = list(color ="#E03C31")) %>% 
    layout(yaxis = list(title = 'Pourcentage %'), barmode = 'stack')
    )

  ################ Par type de véhicule
  
  ##Volume
  
  dfvehic <- baac2018
  dfvehic <- dfvehic %>% group_by(cat_vehic)
  dfvehic <- dfvehic %>% summarize(count = n())
  
  
  output$plot4 <- renderPlotly(
    plot4 <- plot_ly(dfvehic,
                     labels = ~cat_vehic,
                     values = ~count,
                     text = ~paste(cat_vehic),
                     hoverinfo = "text",
                     hovertext = paste("Volume :", dfvehic$count)
    ) %>% 
      add_pie(hole = 0.6) %>% 
      layout(title = "",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
  

  ##Stack  
  

  data_plot3 <- baac2018 %>%
    group_by(cat_vehic, grav) %>%
    summarise(n = n()) %>%
    mutate(freq = round(n / sum(n)*100)) %>%
    select(-n) %>%
    spread(grav, freq) %>%
    select(cat_vehic,
           Tue = "Tué",
           Grave = "Blessé hospitalisé",
           Leger = "Blessé léger",
           Indemne)
  
  data_plot3B<-as.data.frame(data_plot3)
  
  
  output$plot5 <- renderPlotly(
    plot5 <- plot_ly(data_plot3B, x = ~cat_vehic, y = ~Indemne, type = 'bar', name = 'Indemne',hoverinfo='text',text=paste("Indemne", data_plot3B$Indemne,"%" ),marker = list(color ="#407940")) %>% 
      add_trace(y = ~Leger, name = 'Blessé léger',hoverinfo='text',text=paste("Blessé léger", data_plot3B$Leger,"%" ), marker = list(color ="#90EE90")) %>% 
      add_trace(y = ~Grave, name = 'Blessé hospitalisé',hoverinfo='text',text=paste("Blessé hospitalisé", data_plot3B$Grave,"%" ), marker = list(color ="#FCA74F")) %>% 
      add_trace(y = ~Tue, name = 'Tué',hoverinfo='text',text=paste("Tué", data_plot3B$Tue,"%" ), marker = list(color ="#E03C31")) %>% 
      layout(yaxis = list(title = 'Pourcentage %'), barmode = 'stack')
  )

  
  ################ Par port équipement de sécurité
  
  ##Volume
  
  dfsecu <- baac2018
  dfsecu <- dfsecu %>% group_by(utilisation_equipement_secu)
  dfsecu <- dfsecu %>% summarize(count = n())
  
  
  output$plot6 <- renderPlotly(
    plot6 <- plot_ly(dfsecu,
                     labels = ~utilisation_equipement_secu,
                     values = ~count,
                     text = ~paste(utilisation_equipement_secu),
                     hoverinfo = "text",
                     hovertext = paste("Volume :", dfsecu$count)
    ) %>% 
      add_pie(hole = 0.6) %>% 
      layout(title = "",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
  
  
  ##Stack  
  
  
  data_plot4 <- baac2018 %>%
    group_by(utilisation_equipement_secu, grav) %>%
    summarise(n = n()) %>%
    mutate(freq = round(n / sum(n)*100)) %>%
    select(-n) %>%
    spread(grav, freq) %>%
    select(utilisation_equipement_secu,
           Tue = "Tué",
           Grave = "Blessé hospitalisé",
           Leger = "Blessé léger",
           Indemne)
  
  data_plot4B<-as.data.frame(data_plot4)
  
  
  output$plot7 <- renderPlotly(
    plot7 <- plot_ly(data_plot4B, x = ~utilisation_equipement_secu, y = ~Indemne, type = 'bar', name = 'Indemne',hoverinfo='text',text=paste("Indemne", data_plot4B$Indemne,"%" ),marker = list(color ="#407940")) %>% 
      add_trace(y = ~Leger, name = 'Blessé léger',hoverinfo='text',text=paste("Blessé léger", data_plot4B$Leger,"%" ), marker = list(color ="#90EE90")) %>% 
      add_trace(y = ~Grave, name = 'Blessé hospitalisé',hoverinfo='text',text=paste("Blessé hospitalisé", data_plot4B$Grave,"%" ), marker = list(color ="#FCA74F")) %>% 
      add_trace(y = ~Tue, name = 'Tué',hoverinfo='text',text=paste("Tué", data_plot4B$Tue,"%" ), marker = list(color ="#E03C31")) %>% 
      layout(yaxis = list(title = 'Pourcentage %'), barmode = 'stack')
  ) 
  
  
  ################ Par type de route
  
  ##Volume
  
  dfroute <- baac2018
  dfroute <- dfroute %>% group_by(cat_route)
  dfroute <- dfroute %>% summarize(count = n())
  
  
  output$plot8 <- renderPlotly(
    plot8 <- plot_ly(dfroute,
                     labels = ~cat_route,
                     values = ~count,
                     text = ~paste(cat_route),
                     hoverinfo = "text",
                     hovertext = paste("Volume :", dfroute$count)
    ) %>% 
      add_pie(hole = 0.6) %>% 
      layout(title = "",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
  
  
  ##Stack  
  
  
  data_plot5 <- baac2018 %>%
    group_by(cat_route, grav) %>%
    summarise(n = n()) %>%
    mutate(freq = round(n / sum(n)*100)) %>%
    select(-n) %>%
    spread(grav, freq) %>%
    select(cat_route,
           Tue = "Tué",
           Grave = "Blessé hospitalisé",
           Leger = "Blessé léger",
           Indemne)
  
  data_plot5B<-as.data.frame(data_plot5)
  
  
  output$plot9 <- renderPlotly(
    plot9 <- plot_ly(data_plot5B, x = ~cat_route, y = ~Indemne, type = 'bar', name = 'Indemne',hoverinfo='text',text=paste("Indemne", data_plot5B$Indemne,"%" ),marker = list(color ="#407940")) %>% 
      add_trace(y = ~Leger, name = 'Blessé léger',hoverinfo='text',text=paste("Blessé léger", data_plot5B$Leger,"%" ), marker = list(color ="#90EE90")) %>% 
      add_trace(y = ~Grave, name = 'Blessé hospitalisé',hoverinfo='text',text=paste("Blessé hospitalisé", data_plot5B$Grave,"%" ), marker = list(color ="#FCA74F")) %>% 
      add_trace(y = ~Tue, name = 'Tué',hoverinfo='text',text=paste("Tué", data_plot5B$Tue,"%" ), marker = list(color ="#E03C31")) %>% 
      layout(yaxis = list(title = 'Pourcentage %'), barmode = 'stack')
  )  
  
  ################ Par type agglo
  
  ##Volume
  
  dfagglo <- baac2018
  dfagglo <- dfagglo %>% group_by(agg)
  dfagglo <- dfagglo %>% summarize(count = n())
  
  
  output$plot10 <- renderPlotly(
    plot10 <- plot_ly(dfagglo,
                      labels = ~agg,
                      values = ~count,
                      text = ~paste(agg),
                      hoverinfo = "text",
                      hovertext = paste("Volume :", dfagglo$count)
    ) %>% 
      add_pie(hole = 0.6) %>% 
      layout(title = "",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
  
  
  ##Stack  
  
  
  data_plot6 <- baac2018 %>%
    group_by(agg, grav) %>%
    summarise(n = n()) %>%
    mutate(freq = round(n / sum(n)*100)) %>%
    select(-n) %>%
    spread(grav, freq) %>%
    select(agg,
           Tue = "Tué",
           Grave = "Blessé hospitalisé",
           Leger = "Blessé léger",
           Indemne)
  
  data_plot6B<-as.data.frame(data_plot6)
  
  
  output$plot11 <- renderPlotly(
    plot11 <- plot_ly(data_plot6B, x = ~agg, y = ~Indemne, type = 'bar', name = 'Indemne',hoverinfo='text',text=paste("Indemne", data_plot6B$Indemne,"%" ),marker = list(color ="#407940")) %>% 
      add_trace(y = ~Leger, name = 'Blessé léger',hoverinfo='text',text=paste("Blessé léger", data_plot6B$Leger,"%" ), marker = list(color ="#90EE90")) %>% 
      add_trace(y = ~Grave, name = 'Blessé hospitalisé',hoverinfo='text',text=paste("Blessé hospitalisé", data_plot6B$Grave,"%" ), marker = list(color ="#FCA74F")) %>% 
      add_trace(y = ~Tue, name = 'Tué',hoverinfo='text',text=paste("Tué", data_plot6B$Tue,"%" ), marker = list(color ="#E03C31")) %>% 
      layout(yaxis = list(title = 'Pourcentage %'), barmode = 'stack')
  )  
  
  ################ Par IDF/Prov
  
  ##Volume
  
  dfzone <- baac2018
  dfzone <- dfzone %>% group_by(zone)
  dfzone <- dfzone %>% summarize(count = n())
  
  
  output$plot12 <- renderPlotly(
    plot12 <- plot_ly(dfzone,
                      labels = ~zone,
                      values = ~count,
                      text = ~paste(zone),
                      hoverinfo = "text",
                      hovertext = paste("Volume :", dfzone$count)
    ) %>% 
      add_pie(hole = 0.6) %>% 
      layout(title = "",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
  
  
  ##Stack  
  
  
  data_plot7 <- baac2018 %>%
    group_by(zone, grav) %>%
    summarise(n = n()) %>%
    mutate(freq = round(n / sum(n)*100)) %>%
    select(-n) %>%
    spread(grav, freq) %>%
    select(zone,
           Tue = "Tué",
           Grave = "Blessé hospitalisé",
           Leger = "Blessé léger",
           Indemne)
  
  data_plot7B<-as.data.frame(data_plot7)
  
  
  output$plot13 <- renderPlotly(
    plot13 <- plot_ly(data_plot7B, x = ~zone, y = ~Indemne, type = 'bar', name = 'Indemne',hoverinfo='text',text=paste("Indemne", data_plot7B$Indemne,"%" ),marker = list(color ="#407940")) %>% 
      add_trace(y = ~Leger, name = 'Blessé léger',hoverinfo='text',text=paste("Blessé léger", data_plot7B$Leger,"%" ), marker = list(color ="#90EE90")) %>% 
      add_trace(y = ~Grave, name = 'Blessé hospitalisé',hoverinfo='text',text=paste("Blessé hospitalisé", data_plot7B$Grave,"%" ), marker = list(color ="#FCA74F")) %>% 
      add_trace(y = ~Tue, name = 'Tué',hoverinfo='text',text=paste("Tué", data_plot7B$Tue,"%" ), marker = list(color ="#E03C31")) %>% 
      layout(yaxis = list(title = 'Pourcentage %'), barmode = 'stack')
  )  
  
  
  ################ Par Usager
  
  ##Volume
  
  dfcatu <- baac2018
  dfcatu <- dfcatu %>% group_by(catu)
  dfcatu <- dfcatu %>% summarize(count = n())
  
  
  output$plot14 <- renderPlotly(
    plot14 <- plot_ly(dfcatu,
                      labels = ~catu,
                      values = ~count,
                      text = ~paste(catu),
                      hoverinfo = "text",
                      hovertext = paste("Volume :", dfcatu$count)
    ) %>% 
      add_pie(hole = 0.6) %>% 
      layout(title = "",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
  
  
  ##Stack  
  
  
  data_plot8 <- baac2018 %>%
    group_by(catu, grav) %>%
    summarise(n = n()) %>%
    mutate(freq = round(n / sum(n)*100)) %>%
    select(-n) %>%
    spread(grav, freq) %>%
    select(catu,
           Tue = "Tué",
           Grave = "Blessé hospitalisé",
           Leger = "Blessé léger",
           Indemne)
  
  data_plot8B<-as.data.frame(data_plot8)
  
  
  output$plot15 <- renderPlotly(
    plot15 <- plot_ly(data_plot8B, x = ~catu, y = ~Indemne, type = 'bar', name = 'Indemne',hoverinfo='text',text=paste("Indemne", data_plot8B$Indemne,"%" ),marker = list(color ="#407940")) %>% 
      add_trace(y = ~Leger, name = 'Blessé léger',hoverinfo='text',text=paste("Blessé léger", data_plot8B$Leger,"%" ), marker = list(color ="#90EE90")) %>% 
      add_trace(y = ~Grave, name = 'Blessé hospitalisé',hoverinfo='text',text=paste("Blessé hospitalisé", data_plot8B$Grave,"%" ), marker = list(color ="#FCA74F")) %>% 
      add_trace(y = ~Tue, name = 'Tué',hoverinfo='text',text=paste("Tué", data_plot8B$Tue,"%" ), marker = list(color ="#E03C31")) %>% 
      layout(yaxis = list(title = 'Pourcentage %'), barmode = 'stack')
  )     
 
  ################ Par tanche d'age
  
  ##Volume
  
  dfage <- baac2018
  dfage <- dfage %>% group_by(groupe_age)
  dfage <- dfage %>% summarize(count = n())
  
  
  output$plot16 <- renderPlotly(
    plot16 <- plot_ly(dfage,
                      labels = ~groupe_age,
                      values = ~count,
                      text = ~paste(groupe_age),
                      hoverinfo = "text",
                      hovertext = paste("Volume :", dfage$count)
    ) %>% 
      add_pie(hole = 0.6) %>% 
      layout(title = "",  showlegend = F,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)))
  
  
  ##Stack  
  
  
  data_plot9 <- baac2018 %>%
    group_by(groupe_age, grav) %>%
    summarise(n = n()) %>%
    mutate(freq = round(n / sum(n)*100)) %>%
    select(-n) %>%
    spread(grav, freq) %>%
    select(groupe_age,
           Tue = "Tué",
           Grave = "Blessé hospitalisé",
           Leger = "Blessé léger",
           Indemne)
  
  data_plot9B<-as.data.frame(data_plot9)
  
  
  output$plot17 <- renderPlotly(
    plot17 <- plot_ly(data_plot9B, x = ~groupe_age, y = ~Indemne, type = 'bar', name = 'Indemne',hoverinfo='text',text=paste("Indemne", data_plot9B$Indemne,"%" ),marker = list(color ="#407940")) %>% 
      add_trace(y = ~Leger, name = 'Blessé léger',hoverinfo='text',text=paste("Blessé léger", data_plot9B$Leger,"%" ), marker = list(color ="#90EE90")) %>% 
      add_trace(y = ~Grave, name = 'Blessé hospitalisé',hoverinfo='text',text=paste("Blessé hospitalisé", data_plot9B$Grave,"%" ), marker = list(color ="#FCA74F")) %>% 
      add_trace(y = ~Tue, name = 'Tué',hoverinfo='text',text=paste("Tué", data_plot9B$Tue,"%" ), marker = list(color ="#E03C31")) %>% 
      layout(yaxis = list(title = 'Pourcentage %'), barmode = 'stack')
  ) 
  
   
  ##boxplot age  
  
  output$plot20 <- renderPlotly(
    plot20 <- plot_ly(baac2018, y = ~age, color = ~grav, colors = colors_gravite2,type = "box")
  )
 

  ############################################################################## Cartographie---
  
  
  output$map <- renderLeaflet({
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
                title = "Gravité des accidents",
                opacity = 0.6
      )
  })
  
  ############################################################################## Simulateur---- 
  
  
  # On force agg sur certains types de route (car on n'a pas demandé à l'utilisateur de choisir)
  agg_recode <- reactive({
    if (input$cat_route == "Autoroute") {
      agg = "Hors Agglo"
    }
    else if (input$cat_route == "Voie Communale") {
      agg = "Agglo"
    }
    else {
      agg = input$agg
    }
  })
  
  # Calcul du nombre de victimes impliquées
  nb_pers_impl <- reactive({
    if(input$vict1 == TRUE) {vict1 = 1} else {vict1 = 0}
    if(input$vict2 == TRUE) {vict2 = 1} else {vict2 = 0}
    if(input$vict3 == TRUE) {vict3 = 1} else {vict3 = 0}
    if(input$vict4 == TRUE) {vict4 = 1} else {vict4 = 0}
    
    total_victime = vict1 + vict2 + vict3 + vict4
    
    return(total_victime)
  })
  
  # Recodage de la variable Poids Lourd
  presence_PL_recode <- reactive({
    if (input$presence_PL == TRUE) {
      presence_PL_recode = "oui"
    }
    else {
      presence_PL_recode = "non"
    }
  })
  
  # Dataframe de la première victime
  data1 <- reactive({
    data.frame(victime = input$vict1,
               dep = ifelse(str_sub(input$dep, 1, 1) == "0", str_sub(input$dep, 2, 2), str_sub(input$dep, 1, 2)),
               zone = ifelse(str_sub(input$dep, 1, 2) %in% c(75,77,78,91,92,93,94,95), "IDF", "Province"),
               cat_route = input$cat_route,
               agg = agg_recode(),
               collision = input$collision,
               presence_PL = presence_PL_recode(),
               sexe = input$v1_sexe,
               age = input$v1_age,
               cat_vehic = input$v1_cat_vehic,
               catu = input$v1_catu,
               place = input$v1_place,
               loc_pieton = input$v1_loc_pieton,
               nb_pers_impl = nb_pers_impl(),
               utilisation_equipement_secu = ifelse(input$v1_catu == "Piéton", "Non communiqué", input$v1_equiq_secu)
    )
  }) # Fin du Dataframe de la première victime
  
  # Dataframe de la seconde victime
  data2 <- reactive({
    data.frame(victime = input$vict2,
               dep = ifelse(str_sub(input$dep, 1, 1) == "0", str_sub(input$dep, 2, 2), str_sub(input$dep, 1, 2)),
               zone = ifelse(str_sub(input$dep, 1, 2) %in% c(75,77,78,91,92,93,94,95), "IDF", "Province"),
               cat_route = input$cat_route,
               agg = agg_recode(),
               collision = input$collision,
               presence_PL = presence_PL_recode(),
               sexe = input$v2_sexe,
               age = input$v2_age,
               cat_vehic = input$v2_cat_vehic,
               catu = input$v2_catu,
               place = input$v2_place,
               loc_pieton = input$v2_loc_pieton,
               nb_pers_impl = nb_pers_impl(),
               utilisation_equipement_secu = ifelse(input$v2_catu == "Piéton", "Non communiqué", input$v2_equiq_secu)
    )
  }) # Fin du Dataframe de la seconde victime
  
  # Dataframe de la troisième victime
  data3 <- reactive({
    data.frame(victime = input$vict3,
               dep = ifelse(str_sub(input$dep, 1, 1) == "0", str_sub(input$dep, 2, 2), str_sub(input$dep, 1, 2)),
               zone = ifelse(str_sub(input$dep, 1, 2) %in% c(75,77,78,91,92,93,94,95), "IDF", "Province"),
               cat_route = input$cat_route,
               agg = agg_recode(),
               collision = input$collision,
               presence_PL = presence_PL_recode(),
               sexe = input$v3_sexe,
               age = input$v3_age,
               cat_vehic = input$v3_cat_vehic,
               catu = input$v3_catu,
               place = input$v3_place,
               loc_pieton = input$v3_loc_pieton,
               nb_pers_impl = nb_pers_impl(),
               utilisation_equipement_secu = ifelse(input$v3_catu == "Piéton", "Non communiqué", input$v3_equiq_secu)
    )
  }) # Fin du Dataframe de la troisième victime
  
  #Dataframe de la quatrième victime
  data4 <- reactive({
    data.frame(victime = input$vict4,
               dep = ifelse(str_sub(input$dep, 1, 1) == "0", str_sub(input$dep, 2, 2), str_sub(input$dep, 1, 2)),
               zone = ifelse(str_sub(input$dep, 1, 2) %in% c(75,77,78,91,92,93,94,95), "IDF", "Province"),
               cat_route = input$cat_route,
               agg = agg_recode(),
               collision = input$collision,
               presence_PL = presence_PL_recode(),
               sexe = input$v4_sexe,
               age = input$v4_age,
               cat_vehic = input$v4_cat_vehic,
               catu = input$v4_catu,
               place = input$v4_place,
               loc_pieton = input$v4_loc_pieton,
               nb_pers_impl = nb_pers_impl(),
               utilisation_equipement_secu = ifelse(input$v4_catu == "Piéton", "Non communiqué", input$v4_equiq_secu)
    )
  }) # Fin du Dataframe de la quatrième victime
  
  # Ajout des datas supplémentaires
  data1b <- reactive({
    data1() %>%
      left_join(alcool_et_accidents, by="dep")
  })
  
  data2b <- reactive({
    data2() %>%
      left_join(alcool_et_accidents, by="dep")
  })
  
  data3b <- reactive({
    data3() %>%
      left_join(alcool_et_accidents, by="dep")
  })
  
  data4b <- reactive({
    data4() %>%
      left_join(alcool_et_accidents, by="dep")
  })
  
  observeEvent(input$bouton_calcul,{ #Clic sur le bouton
    # Prediction pour la premième victime
    v1_calcul_pred <- reactive({
      if (input$v1_catu == 'Conducteur') {
        pred = predict(modele_conduc, newdata = data1b())
      }
      else if (input$v1_catu == 'Passager') {
        pred = predict(modele_passager, newdata = data1b())
      }
      else if (input$v1_catu == 'Piéton') {
        pred = predict(modele_pieton, newdata = data1b())
      }
      return(pred$data[,c("response")])
    })
    #output$strfile <- renderPrint({str(v1_calcul_pred())})
    
    # Prediction pour la seconde victime
    v2_calcul_pred <- reactive({
      if (input$v2_catu == 'Conducteur') {
        pred = predict(modele_conduc, newdata = data2b())
      }
      else if (input$v2_catu == 'Passager') {
        pred = predict(modele_passager, newdata = data2b())
      }
      else if (input$v2_catu == 'Piéton') {
        pred = predict(modele_pieton, newdata = data2b())
      }
      return(pred$data[,c("response")])
    })
    
    # Prediction pour la troisième victime
    v3_calcul_pred <- reactive({
      if (input$v3_catu == 'Conducteur') {
        pred = predict(modele_conduc, newdata = data3b())
      }
      else if (input$v3_catu == 'Passager') {
        pred = predict(modele_passager, newdata = data3b())
      }
      else if (input$v3_catu == 'Piéton') {
        pred = predict(modele_pieton, newdata = data3b())
      }
      return(pred$data[,c("response")])
    })
    
    # Prediction pour la quatrième victime
    v4_calcul_pred <- reactive({
      if (input$v4_catu == 'Conducteur') {
        pred = predict(modele_conduc, newdata = data4b())
      }
      else if (input$v4_catu == 'Passager') {
        pred = predict(modele_passager, newdata = data4b())
      }
      else if (input$v4_catu == 'Piéton') {
        pred = predict(modele_pieton, newdata = data4b())
      }
      return(pred$data[,c("response")])
    })
    
    # Affichage de la Prédiction pour la Victime N°1 quand on clique sur le bouton
    if (input$vict1 == TRUE) {
      output$v1_prediction <- renderUI({
        input$bouton_calcul
        isolate({
          if(v1_calcul_pred() == "Indemne") {
            v1_affichage_reponse <- paste("<span style='background-color:#32CD32;float:center;color:white;font-size: 12px;border: 5px solid #32CD32;border-radius: 3px'>", v1_calcul_pred(), "</span>")
          }
          else if(v1_calcul_pred() == "Blessé léger") {
            v1_affichage_reponse <- paste("<span style='background-color:#ff8c00;float:center;color:white;font-size: 12px;border: 5px solid #ff8c00;border-radius: 3px'>", v1_calcul_pred(), "</span>")
          }
          else {
            v1_affichage_reponse <- paste("<span style='background-color:#b20000;float:center;color:white;font-size: 12px;border: 5px solid #b20000;border-radius: 3px'>", v1_calcul_pred(), "</span>")
          }
          HTML(v1_affichage_reponse)
        })
      })
    } # Fin de Affichage de la Prédiction pour la Victime N°1 quand on clique sur le bouton
    
    # Affichage de la Prédiction pour la Victime N°2 quand on clique sur le bouton
    if (input$vict2 == TRUE) {
      output$v2_prediction <- renderUI({
        input$bouton_calcul
        isolate({
          if(v2_calcul_pred() == "Indemne") {
            v2_affichage_reponse <- paste("<span style='background-color:#32CD32;float:center;color:white;font-size: 12px;border: 5px solid #32CD32;border-radius: 3px'>", v2_calcul_pred(), "</span>")
          }
          else if(v2_calcul_pred() == "Blessé léger") {
            v2_affichage_reponse <- paste("<span style='background-color:#ff8c00;float:center;color:white;font-size: 12px;border: 5px solid #ff8c00;border-radius: 3px'>", v2_calcul_pred(), "</span>")
          }
          else {
            v2_affichage_reponse <- paste("<span style='background-color:#b20000;float:center;color:white;font-size: 12px;border: 5px solid #b20000;border-radius: 3px'>", v2_calcul_pred(), "</span>")
          }
          HTML(v2_affichage_reponse)
        })
      })
    } # Fin de Affichage de la Prédiction pour la Victime N°2 quand on clique sur le bouton
    
    # Affichage de la Prédiction pour la Victime N°3 quand on clique sur le bouton
    if (input$vict3 == TRUE) {
      output$v3_prediction <- renderUI({
        input$bouton_calcul
        isolate({
          if(v3_calcul_pred() == "Indemne") {
            v3_affichage_reponse <- paste("<span style='background-color:#32CD32;float:center;color:white;font-size: 12px;border: 5px solid #32CD32;border-radius: 3px'>", v3_calcul_pred(), "</span>")
          }
          else if(v3_calcul_pred() == "Blessé léger") {
            v3_affichage_reponse <- paste("<span style='background-color:#ff8c00;float:center;color:white;font-size: 12px;border: 5px solid #ff8c00;border-radius: 3px'>", v3_calcul_pred(), "</span>")
          }
          else {
            v3_affichage_reponse <- paste("<span style='background-color:#b20000;float:center;color:white;font-size: 12px;border: 5px solid #b20000;border-radius: 3px'>", v3_calcul_pred(), "</span>")
          }
          HTML(v3_affichage_reponse)
        })
      })
    } # Fin de Affichage de la Prédiction pour la Victime N°3 quand on clique sur le bouton
    
    # Affichage de la Prédiction pour la Victime N°4 quand on clique sur le bouton
    if (input$vict4 == TRUE) {
      output$v4_prediction <- renderUI({
        input$bouton_calcul
        isolate({
          if(v4_calcul_pred() == "Indemne") {
            v4_affichage_reponse <- paste("<span style='background-color:#32CD32;float:center;color:white;font-size: 12px;border: 5px solid #32CD32;border-radius: 3px'>", v4_calcul_pred(), "</span>")
          }
          else if(v4_calcul_pred() == "Blessé léger") {
            v4_affichage_reponse <- paste("<span style='background-color:#ff8c00;float:center;color:white;font-size: 12px;border: 5px solid #ff8c00;border-radius: 3px'>", v4_calcul_pred(), "</span>")
          }
          else {
            v4_affichage_reponse <- paste("<span style='background-color:#b20000;float:center;color:white;font-size: 12px;border: 5px solid #b20000;border-radius: 3px'>", v4_calcul_pred(), "</span>")
          }
          HTML(v4_affichage_reponse)
        })
      })
    } # Fin de Affichage de la Prédiction pour la Victime N°4 quand on clique sur le bouton
    
    # Message d'erreur en cas d'oubli de la première victime
    if(input$vict1 == FALSE) {
      showModal(modalDialog(
        title = "Avertissement",
        "Etes-vous sûr d'avoir saisi les informations de la victime n°1? ",
        easyClose = TRUE,
        footer = tagList(modalButton("Fermer"))
      ))
    }
    
    # Message d'erreur en cas d'oubli de la seconde victime
    else if(input$vict2 == FALSE & (input$vict3 == TRUE | input$vict4 == TRUE)) {
      showModal(modalDialog(
        title = "Avertissement",
        "Etes-vous sûr d'avoir saisi les informations de la victime n°2? ",
        easyClose = TRUE,
        footer = tagList(modalButton("Fermer"))
      ))
    }
    
    # Message d'erreur en cas d'oubli de la troisième victime
    else if(input$vict3 == FALSE & input$vict4 == TRUE) {
      showModal(modalDialog(
        title = "Avertissement",
        "Etes-vous sûr d'avoir saisi les informations de la victime n°3? ",
        easyClose = TRUE,
        footer = tagList(modalButton("Fermer"))
      ))
    }
  }) # fin du observe event (Clic sur le bouton de calcul)
  
  output$table <- renderTable({data1b()})
  
})
