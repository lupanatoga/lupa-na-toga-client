library(shiny)
library(plotly)
# Define UI
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Lupa na Toga"),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput("orgao"),
      uiOutput("mes")
    ),
    
    # Show a plot of the generated graph
    mainPanel(
      plotlyOutput("pointsPlot"),
      plotlyOutput("linesPlot")
    )
  )

  
))
