library(shiny)
library(tidyverse, warn.conflicts = F)
library(plotly)
theme_set(theme_bw())

data = read_csv("./salarios-magistrados-2018-02-05.csv")
shinyServer(function(input, output) {
  output$pointsPlot <- renderPlotly({
    plot_ly(data = data %>% select(subsidio, indenizacoes), 
            y = ~ indenizacoes, 
            x = ~ subsidio, 
            name = "Haters",
            type = "scatter",
            hoverinfo = 'text',
            marker = list(size = 12))
  })
  output$orgao <- renderUI({
    selectInput("Orgão", "Selecione um Orgão", data$orgao)
  })
  
  output$lotacao <- renderUI({
    selectInput("Lotação", "Selecione uma Lotação", data$lotacao)
  })
})
