library(shiny)
library(ggplot2)
library(dplyr)

data(iris)

ui <- fluidPage(
  titlePanel("Iris Dataset Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("xcol", "X-axis:", choices = names(iris)[1:4], selected = "Sepal.Length"),
      selectInput("ycol", "Y-axis:", choices = names(iris)[1:4], selected = "Sepal.Width"),
      checkboxGroupInput("species", "Select Species:", choices = levels(iris$Species),
                         selected = levels(iris$Species))
    ),
    
    mainPanel(
      plotOutput("scatterPlot"),
      tableOutput("summaryTable")
    )
  )
)

server <- function(input, output) {
  filtered <- reactive({
    iris %>% filter(Species %in% input$species)
  })
  
  output$scatterPlot <- renderPlot({
    ggplot(filtered(), aes_string(x = input$xcol, y = input$ycol, color = "Species")) +
      geom_point(size = 3) + theme_minimal()
  })
  
  output$summaryTable <- renderTable({
    filtered() %>% group_by(Species) %>%
      summarise(across(where(is.numeric), mean, .names = "avg_{col}"))
  })
}

shinyApp(ui = ui, server = server)
