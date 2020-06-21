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
# library(plotly)
# library(DT)


# Define UI for dataset viewer app ----
ui <- fluidPage(theme = shinytheme("cerulean"),
                
                # App title ----
                titlePanel("Stats Déscriptives"),
                
                # Sidebar layout with a input and output definitions ----
                sidebarLayout(
                  
                  # Sidebar panel for inputs ----
                  sidebarPanel(width=4,
                               h4("Selection Dataset/variables"), 
                               # Input: Selector for choosing dataset ----
                               selectInput(inputId = "dataset",
                                           label = "Selection du Dataset:",
                                           choices = c("Train", "Test")),
                               
                               # selectInput(inputId = "datavar",
                               #             label = "Choose a variable",
                               #             choices = NULL),
                               hr(color="blue" ),
                               h4("Option du graph :"),
                               selectInput(inputId="Plot_type", label="Type de graphe:", list('','Histogram', 'Scatterplot', 'Boxplot'), multiple=FALSE),
                               uiOutput("x"),
                               uiOutput("y"),
                               #selectInput("variable","Variable:", choices= colnames(datasetInput)),
                               
                               uiOutput("hist_type2"), 
                               uiOutput("group2"),
                               uiOutput("box_ratio"),
                               uiOutput("split"),
                               hr(),
                               h4("Ajustement du graph :"), ## Text in sidebarPanel ##
                               numericInput(inputId="hei", label="Height", value=800, min=500, max=2000), ## Height ##
                               numericInput(inputId="wid", label="Width", value=1000, min=500, max=2000), 
                               
                               
                               # Input: Numeric entry for number of obs to view ----
                               numericInput(inputId = "obs",
                                            label = "Nombre d'observations:",
                                            value = 10)),
                  
                  
                  # uiOutput(outputId = "columns"),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(width=8,
                            tabsetPanel(type = "tabs",
                                        tabPanel("Dataset", tableOutput("view"),verbatimTextOutput("summary"),class = 'rightAlign'),
                                        tabPanel("Plot", plotOutput(outputId = "plot", height="auto", width="auto")),
                                        tabPanel("CrossTab", tags$head(tags$style(type = 'text/css','#myPivot{ overflow-x: scroll; }')),
                                                 rpivotTableOutput("myPivot", width = "100%", height = "700px")),
                                        tabPanel("DashBoard",dashboardPage(
                                          dashboardHeader(),
                                          dashboardSidebar(),
                                          dashboardBody()
                                        ))
                                        
                            )
                            
                            
                            # Output: Verbatim text for data summary ----
                            #verbatimTextOutput("summary")
                            #verbatimTextOutput("summary2"),
                            
                            # Output: HTML table with requested number of observations ----
                            #tableOutput("view")
                            
                  )
                )
)



  
load(file="Shiny/data_baac.RData")

baa2017_bis <- baac2017 %>%
  group_by(catu, grav) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))


plot_test<- plot_ly(
  baa2017_bis,
  x = ~ catu,
  color = ~ grav,
  colors = brewer.pal(5,'RdYlBu'),
  #y = ~sexe,
  stroke = I("black"),
  type = "histogram") %>%
  layout(yaxis = list(title = 'Count'), barmode = "stack")
  
plot_test  


######




sexe <- c("Homme", "Femme")
Indemne <- c(20, 14)
Tue <- c(30, 5)
Leger<- c(20, 20)
Grave<- c(50, 20)

data <- data.frame(sexe, Indemne,Tue,Leger,Grave)

data2 <- baac2017 %>%
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



fig <- plot_ly(data2, x = ~sexe, y = ~Indemne, type = 'bar', name = 'Indemne',marker = list(color ="1f77b4"))
fig <- fig %>% add_trace(y = ~Tue, name = 'Tué',marker = list(color ="ff7f0e"))
fig <- fig %>% add_trace(y = ~Leger, name = 'Blessé léger',marker = list(color ="2ca02c"))
fig <- fig %>% add_trace(y = ~Grave, name = 'Blessé hospitalisé',marker = list(color ="d62728"))
fig <- fig %>% layout(yaxis = list(title = 'Pourcentage%'), barmode = 'stack')

fig



#1f77b4 or rgb(31, 119, 180)  // muted blue
#ff7f0e or rgb(255, 127, 14)  // safety orange
#2ca02c or rgb(44, 160, 44)   // cooked asparagus green
#d62728 or rgb(214, 39, 40)   // brick red
#9467bd or rgb(148, 103, 189) // muted purple
#8c564b or rgb(140, 86, 75)   // chestnut brown


str(baac2017$grav)
summary(data2)


Produits<-c("Crème de jour","sérum","Crème de nuit","masque","démaquillant à rincer",
            "démaquillant sans rincage","lotion","eau florale","huile","produits teintés")
Pourcentages<-c(27.1,14.5,13.8,8.82,7.73,7.24,6.57,5.83,5.65,2.82)
colors<-c("#ff0000","#ff1919","green","#ff4c4c","#ff6666",
          "#ff7f7f","#ff9999","#ffb2b2","#ffcccc","#ffe5e5")

Data<-data.frame(Produits,Pourcentages,colors)


plot_ly(Data, labels = ~Produits, values = ~Pourcentages, type = 'pie', marker = list(colors = ~colors))%>%
  layout(title = 'Les pourcentages des types de soins préférés',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         showlegend = TRUE)

#################################################

colors_gravite <- c("#FCA74F","#90EE90","#407940","#E03C31")
colors_gravite2 <- c("#407940","#90EE90","#FCA74F","#E03C31")
colors_gravite2 <- c("#29CF56","#A5F4A5","#FCA74F","#E03C31")

#29CF56 vert foncé
#A5F4A5 vert clair


data_plot1 <- baac2017 %>%
  group_by(grav) %>%
  summarise(n = n()) %>%
  mutate(Pourcentages = round(n / sum(n)*100,1))


data_plot1B<-data.frame(data_plot1,colors_gravite)

plot_ly(data_plot1B,
        labels = ~grav,
        values = ~Pourcentages, 
        type = 'pie',
        textposition = 'inside',
        textinfo = 'label+percent',
        text = ~paste(grav),
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = "text",
        hovertext = paste("Volume :", data_plot2$n),
        marker = list(colors = ~colors_gravite)) %>%
  layout(title = '',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         showlegend = FALSE
  )


##############################################


df <- baac2018
df <- df %>% group_by(sexe)
df <- df %>% summarize(count = n())


fig <- df %>% plot_ly(labels = ~sexe,
                      values = ~count,
                      text = ~paste(sexe),
                      hoverinfo = "text",
                      hovertext = paste("Volume :", df$count)
                      )

fig <- fig %>% add_pie(hole = 0.6)

fig <- fig %>% layout(title = "",  showlegend = F,
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig


##############################################


data2 <- baac2017 %>%
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

data2<-as.data.frame(data2)

fig <- plot_ly(data2, x = ~sexe, y = ~Indemne, type = 'bar', name = 'Indemne',hoverinfo='text',text=paste("Indemne", data2$Indemne,"%" ),marker = list(color ="#407940"))
fig <- fig %>% add_trace(y = ~Leger, name = 'Blessé léger',hoverinfo='text',text=paste("Blessé léger", data2$Leger,"%" ), marker = list(color ="#90EE90"))
fig <- fig %>% add_trace(y = ~Grave, name = 'Blessé hospitalisé',hoverinfo='text',text=paste("Blessé hospitalisé", data2$Grave,"%" ), marker = list(color ="#FCA74F"))
fig <- fig %>% add_trace(y = ~Tue, name = 'Tué',hoverinfo='text',text=paste("Tué", data2$Tue,"%" ), marker = list(color ="#E03C31"))
fig <- fig %>% layout(yaxis = list(title = 'Pourcentage %'), barmode = 'stack')

fig

rm(data2)


###################################################



baac2018$grav<- fct_relevel(baac2018$grav, "Indemne", "Blessé léger", "Blessé hospitalisé", "Tué")

plot10 <- plot_ly(baac2018, y = ~age, color = ~grav, colors = colors_gravite2,type = "box")

plot10