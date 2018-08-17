library(shiny)
library(tidyverse, warn.conflicts = F)
library(plotly)

data = read_csv("./salarios-magistrados-2018-02-05.csv")
data_1 = read_csv("./salarios-magistrados-2018-02-05.csv") 

data = data %>% 
    mutate(total = rendimento_liquido + diarias, mes = 12) %>% 
  filter(total >= 0)

data_1 = data_1 %>% 
  mutate(total = rendimento_liquido + diarias, mes = 11) %>% 
  filter(total >= 0)

salarios <- bind_rows(data, data_1)
salarios$mes <- as.character(salarios$mes)

shinyServer(function(input, output) {

  sumario <- salarios %>% group_by(nome) %>% summarise(auxilio = sum(total))
  
  output$pointsPlot <- renderPlotly({
    plot_ly(data = sumario, 
            x = ~ auxilio, 
            y = 0,
            hoverinfo = 'text',
            source = "juiz",
            text = ~ nome
            )
  })
  
  output$linesPlot = renderPlotly({
    s <- event_data("plotly_click", source = "juiz")
    if(length(s)) {
      juiz = sumario[s[["pointNumber"]]+1,]
      salarios_um = salarios %>% filter(nome == juiz$nome)
    
      plot_ly(data = salarios_um,
              y = ~ total,
              x = ~ mes,
              type = "scatter",
              mode = "lines"
      )
    } else {
      plotly_empty()
    }
  })
  
  output$treemap  = renderPlotly({
    s <- event_data("plotly_click", source = "juiz")
    if(length(s)) {
      
    } else {
      plotly_empty()
    }
  })
  
  
  output$orgao <- renderUI({
    selectInput("Orgão", "Selecione um Orgão", data$orgao)
  })
  
  output$lotacao <- renderUI({
    selectInput("Lotação", "Selecione uma Lotação", data$lotacao)
  })
  
  output$mes <- renderUI({
    selectInput("Mes", "Mes referente", c('jan', 'fev', 'mar'))
  })
})
