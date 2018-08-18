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
  
  output$fistPlot <- renderSunburst({
    d <- data.frame(group = c("subsidio","direitos_pessoais-abono_de_permanencia", "direitos_pessoais-subsidio_outra1", "indenizacoes-ajuda_de_custo","indenizacoes-indenizacoes_outros","direitos_eventuais-indenizacao_de_ferias","direitos_eventuais-abono_contitucional_de_1_3_de_ferias","direitos_eventuais-antecipacao_de_ferias","direitos_eventuais-gratificacao_natalina","direitos_eventuais-antecipacao_de_gratificacao_natalina","direitos_eventuais-substituicao","direitos_eventuais-gratificacao_por_exercicio_cumulativo","direitos_eventuais-gratificacao_por_encargo_curso_concurso","direitos_eventuais-pagamento_em_retroativos","direitos_eventuais-jeton", "indenizacoes-auxilio-auxilio_alimentacao", "indenizacoes-auxilio-auxilio_pre_escolar", "indenizacoes-auxilio-auxilio_saude", "indenizacoes-auxilio-auxilio_natalidade", "indenizacoes-auxilio-auxilio_moradia"),
                        value = c(sum(data$subsidio), sum(data$abono_de_permanencia), sum(data$subsidio_outra1),sum(data$ajuda_de_custo),sum(data$indenizacoes_outra1 + data$indenizacoes_outra2 + data$indenizacoes_outra3),sum(data$indenizacao_de_ferias),sum(data$abono_contitucional_de_1_3_de_ferias) ,sum(data$antecipacao_de_ferias) ,sum(data$gratificacao_natalina),sum(data$antecipacao_de_gratificacao_natalina),sum(data$substituicao),sum(data$gratificacao_por_exercicio_cumulativo),sum(data$gratificacao_por_encargo_curso_concurso),sum(data$pagamento_em_retroativos),sum(data$jeton),sum(data$auxilio_alimentacao),sum(data$auxilio_pre_escolar), sum(data$auxilio_saude), sum(data$auxilio_natalidade),sum(data$auxilio_moradia)))
    sunburst(d,count = TRUE)  
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
  
  get_lotacao = function() {
    return( data_ %>% 
        filter(nome %in% sumario$nome)) %>% 
        select(lotacao) %>% 
        distinct()
  }
  
  get_cargo = function() {
    return( data_ %>% 
              filter(nome %in% sumario$nome)) %>% 
      select(cargo) %>% 
      distinct()
  }
  
  output$orgao <- renderUI({
      selectInput("orgao", "Selecione um Orgão", data$orgao)
  })
  
  output$lotacao <- renderUI({
    if(str_length(input$orgao) > 0) {
      selectInput("lotacao", "Selecione uma Lotação", data$lotacao)
    } else {
      selectInput("lotacao", "Selecione uma Lotação", c())
    }
  })
  
  output$cargo <- renderUI({
    if(str_length(input$lotacao) > 0 && str_length(input$orgao) > 0) {
      selectInput("cargo", "Cargo", data$cargo)
    } else {
      selectInput("cargo", "Cargo", c())
    }
  })
  
})
