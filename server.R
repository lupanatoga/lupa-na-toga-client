library(shiny)
library(tidyverse, warn.conflicts = F)
library(plotly)
library(sunburstR)
library(dplyr)
library(jsonlite)

data = fromJSON("sample.txt") %>%  as_data_frame()

data = data %>% 
    mutate(total = rendimento_liquido + diarias, mes = strsplit(mes_ano_referencia, "/")[[1]][1]) %>% 
  filter(total >= 0)

data_1 = data

data_1 = data_1 %>%  mutate(mes = as.character(as.numeric(mes)+1))

data = bind_rows(data, data_1)

sumario <- data %>% group_by(nome) %>% summarise(auxilio = sum(total))
shinyServer(function(input, output) {
  
  
  output$pointsPlot <- renderPlotly({
    if(str_length(input$lotacao)) {
      data_ = data %>%  filter(lotacao == input$lotacao)
    } else {
      data_ = data
    }
    
    if(str_length(input$orgao)) {
      data_ = data_ %>%  filter(orgao == input$orgao)
    } else {
      data_ = data_
    }
    
    if(str_length(input$cargo)) {
      data_ = data_ %>%  filter(cargo == input$cargo)
    } else {
      data_ = data_
    }
    
    sumario <- data_ %>% group_by(nome) %>% summarise(auxilio = sum(total))
    
    
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
      salarios_um = data %>% filter(nome == juiz$nome) %>%  select(total, mes)
      plot_ly(data = salarios_um,
              y = ~ total,
              x = ~ mes,
              type = "scatter",
              source = "temporal",
              mode = "lines"
      )
    } else {
      plotly_empty()
    }
  })
  
  selection = reactive({
    input$sunburst_mouseover
  })
  
  output$selection = renderText(selection())
  
  output$sumburstPlot  = renderSunburst({
    s <- event_data("plotly_click", source = "juiz")
    s2 <- event_data("plotly_click", source = "temporal")
    if(length(s)) {
      juiz = sumario[s[["pointNumber"]]+1,]
      
      if(length(s2)) {
        salarios_temp = data %>%filter(mes == s2[["x"]])
      } else {
        salarios_temp = data
      }
      
      a = salarios_temp %>% filter(nome == juiz$nome) %>% select(indenizacoes, direitos_pessoais, direitos_eventuais)
      group = colnames(a)
      a = t(a) %>% as_data_frame() 
      a$group = group
      a = a%>% rename(value = V1)
      a$group <- as.factor(a$group)
      
      a = a %>% select(group, value)
      
      sunburst(a)
    } else {
      sunburst(df[FALSE,])
    }
  })
  
  
  output$orgao <- renderUI({
    selectInput("orgao", "Selecione um Orgão", data$orgao)
  })
  
  output$lotacao <- renderUI({
    selectInput("lotacao", "Selecione uma Lotação", data$lotacao)
  })
  
  output$cargo <- renderUI({
    selectInput("cargo", "Cargo", data$cargo)
  })
})
