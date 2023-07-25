library(sp)
library(ggplot2)
library(dplyr)
library(shiny)
library(plotly)
library(rnaturalearth)
library(rnaturalearthdata)



# Chargement des données géographiques de l'Afrique de l'Ouest
afrique_ouest <- subset(ne_countries(scale = "medium", continent = "Africa"), subregion == "Western Africa")
donnees<-read.csv("ACLED-Western_Africa.csv")

evenement_filtre<- subset(donnees, pays=="Mali" & type=="Protests" & annee=="2022")
# Création d'un dataframe pour les marqueurs
ui <- fluidPage(
  
  # titre de l'application
  titlePanel("Carte d'Afrique de l'Ouest"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId="pays",
        label="Sélectionnez un pays ou plusieurs pays",
        choices=c(unique(donnees$pays)),
        selected = c(unique(donnees$pays))[sample(1:length(unique(base$pays)),1)],
        multiple = TRUE,
        
      ),
      selectInput(
        inputId="annee",
        label="Sélectionnez un ou plusieurs années",
        choices=c(unique(donnees$annee)),
        selected = "Protests",
        multiple = TRUE
      ),
      selectInput(
        inputId="annee",
        label="Sélectionnez une annee",
        choices=c(unique(base$annee)),
        selected = "2023",
        multiple = TRUE
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput(outputId="map",
                   width = "100%",
                   height = "720px")
    )
  )
)

server <- function(input, output, session) {
  base_fil<-reactive({
    base %>% 
      dplyr::filter(pays %in% input$pays ) %>% 
      dplyr::filter(type %in% input$evenement ) %>%
      dplyr::filter(annee %in% input$annee ) 
    
  }  )
  output$map <- renderPlotly({
    gg <- ggplot() +
      geom_polygon(data = ne_countries(type = "countries",country = c(input$pays)), aes(x = long, y = lat, group = group),
                   fill = "lightblue", color = "gray", alpha = 0.6) +
      geom_point(data = base_fil(), aes(x = longitude, y = latitude),
        size = 3, alpha = 0.7) +
      theme_void() +
      labs(title = "Carte de l'Afrique de l'Ouest", x = "", y = "") +
      theme(legend.position = "bottom")
    
    ggplotly(gg)
  })
}

shinyApp(ui = ui, server = server)
