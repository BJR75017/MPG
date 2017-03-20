library(shiny)
library(shinydashboard)
load("~/MPG/mpg_moulinette/data/results.RData")
source("~/MPG/mpg_moulinette/library.R")


sidebar <-  dashboardSidebar(
                menuItem("Mercato", tabName = "mercato", icon = icon("soccer-ball-o")),
                menuItem("Equipe", tabName = "equipe", icon = icon("group"))
             )
  
body <- dashboardBody(
    tabItems(
      tabItem(tabName = "mercato",
              h2("Aide au mercato"),
              
              # Sélection du poste
              fluidRow(
                box(
                  title = "Sélectionnez un poste",
                  selectInput("poste", label = "",
                              choices = list("Attaquant" = "A", "Gardien" = "G", "Défenseur" = "D", "Milieu" = "M"),
                              selected = 1)
                ),
                box(
                  title = "Sélectionnez un joueur",
                  uiOutput("name_filter")
                )
              ),
              
              # Affichage de la liste
              fluidRow(
                box(tableOutput("mpg_filters")),
                box(plotOutput("stat_player"))
              )
      ),
      
      tabItem(tabName = "equipe",
              h2("Composition de l'équipe"))
))


ui <- dashboardPage(skin = "green", dashboardHeader(title = "MPG"), sidebar, body)
  
    
server <- function(input, output) {
  
  output$mpg_filters <- reactiveTable(function() {
    filter_order(results_mpg, input$poste)
  })
  
  
  output$name_filter <- renderUI({
    selectInput("joueur", label = "",
                choices = filter_order_name(results_mpg, input$poste))
  })
  
  output$stat_player<-renderPlot({
    stat_graph(results_mpg, input$poste, input$joueur)
  })

  
}

shinyApp(ui, server)