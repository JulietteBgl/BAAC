#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  
  #A lancer si besoin :  
  #load(file="app/carto.RData")
  
  #Return the requested dataset ----
  datasetInput <- reactive({
    switch(input$dataset,
           "2017" = baac2017,
           "2018" = baac2018
    )
  })
  
  # summary du dataset ----
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  # vue des 10 premiéres obs ----
  output$view <- renderTable({
    head(datasetInput(), n = 10)
  })
  
  # Croiseur----
  output$myPivot <- renderRpivotTable(rpivotTable(data = datasetInput(),rows = "sexe", cols = "grav", aggregatorName="Count as Fraction of Columns"))
  
  output$plot2 <- renderPlotly(
    plot2 <- plot_ly(
      datasetInput(),
      x = ~ sexe,
      color = ~ grav,
      #colors = brewer.pal(5,'RdYlBu'),
      #y = ~sexe,
      stroke = I("black"),
      type = "histogram"
    )
  )
  
  
  #colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
  

  output$plot1 <- renderPlotly(
    plot1 <- plot_ly(datasetInput(), labels = ~grav, type = 'pie',
                     textposition = 'inside',
                     textinfo = 'label+percent',
                     insidetextfont = list(color = '#FFFFFF'),
                     hoverinfo = 'text',
                     text = ~paste(grav),
                     marker = list(colors = brewer.pal(5,'RdYlGn'),
                                   line = list(color = '#FFFFFF', width = 1)), showlegend = FALSE) %>%
      layout(
        title = '',
        xaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        ),
        yaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE
        )
      ))
  
  output$plot3 <- renderPlotly(
    plot3 <- plot_ly(
      datasetInput(),
      x = ~ catu,
      color = ~ grav,
      colors = brewer.pal(5,'RdYlBu'),
      #y = ~sexe,
      stroke = I("black"),
      type = "histogram"
    )
  )
  
  output$plot4 <- renderPlotly(
    plot4 <- plot_ly(
      datasetInput(),
      x = ~ cat_vehic,
      color = ~ grav,
      colors = brewer.pal(5,'RdYlBu'),
      #y = ~sexe,
      stroke = I("black"),
      type = "histogram"
    )
  )
  
  output$plot5 <- renderPlotly(
    plot5 <- plot_ly(
      datasetInput(),
      x = ~ utilisation_equipement_secu,
      color = ~ grav,
      colors = brewer.pal(5,'RdYlBu'),
      #y = ~sexe,
      stroke = I("black"),
      type = "histogram"
    )
  )
  
  output$plot6 <- renderPlotly(
    plot5 <- plot_ly(
      datasetInput(),
      x = ~ agg,
      color = ~ grav,
      colors = brewer.pal(5,'RdYlBu'),
      #y = ~sexe,
      stroke = I("black"),
      type = "histogram"
    )
  )
  
  
  output$plot7 <- renderPlotly(
    plot7 <- plot_ly(
      datasetInput(),
      x = ~ zone,
      color = ~ grav,
      colors = brewer.pal(5,'RdYlBu'),
      #y = ~sexe,
      stroke = I("black"),
      type = "histogram"
    )
  )  
  
  
  output$plot10 <- renderPlotly(
    plot10 <- plot_ly(datasetInput(), y = ~age, color = ~grav, colors = brewer.pal(5,'RdYlBu'),type = "box")
  ) 
  
  output$map <- renderLeaflet({
    france_dep %>% 
      filter(nom != c("Mayotte","La Réunion","Guyane","Guadeloupe","Martinique","Corse-du-Sud","Haute-Corse")) %>%   # pas de données pour Mayotte
      st_simplify(preserveTopology = TRUE, dTolerance = 0.01) %>% 
      left_join(carto2017, by = c("code_insee" = "dep")) %>% 
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
  
}


