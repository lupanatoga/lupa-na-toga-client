library(sunburstR)
library(shiny)
library(tidyverse)
library(plotly)
library(dplyr)
library(jsonlite)
library(readr)
library(zoo)
library(htmltools)
library(lubridate)

data = read_csv("salarios_tratados_com_mes.csv")
data$mes_ano_referencia <- paste0("01/",data$mes_ano_referencia )
data$mes_ano_referencia <- dmy(data$mes_ano_referencia)
salarios_t = data
salarios_t[is.na(salarios_t)] = 0

n=3
teto = 33700

n_mes = unique(salarios_t$mes_ano_referencia) %>% length()

total_rendimentos = sum(salarios_t$rendimento_liquido)
total_rendimentos_mes = total_rendimentos/n_mes
total_rendimentos_mes_milhoes = as.integer((total_rendimentos/n_mes) / 1000000)

acima = salarios_t$rendimento_liquido - teto


porcentagem = as.integer((total_rendimentos/(nrow(salarios_t)*teto))*100)

max_mes_ano_referencia = "04/18"

sumario <- data %>% group_by(nome) %>% summarise(auxilio = sum(total))
shinyServer(function(input, output) {
  output$his_jui  <- renderTable({
    salarios_t %>% arrange(-rendimento_liquido) %>% slice(1:n) %>% select(nome, mes_ano_referencia, rendimento_liquido)
  })
  
  output$plot_total_rendimentos_mes <- renderText({
    total_rendimentos_mes
  })
  
  output$plot_total_rendimentos_mes_milhoes <- renderText({
    total_rendimentos_mes_milhoes
  })
  
  output$plot_porcentagem <- renderText({
    porcentagem
  })
  
  output$his_jui_last <- renderTable({
    salarios_t %>% filter(mes_ano_referencia == max_mes_ano_referencia) %>% arrange(-rendimento_liquido) %>% slice(1:n) %>% select(nome, mes_ano_referencia, rendimento_liquido)
  })
  
  output$his_jui_mochi <- renderTable({
    salarios_t %>% group_by(nome) %>% summarise(aux_diarias = sum(diarias)) %>% arrange(-aux_diarias) %>% slice(1:n)
  })
  
  output$magis_laje <- renderTable({
    salarios_t %>% group_by(nome) %>% summarise(aux_moradia = sum(auxilio_moradia)) %>% 
      arrange(-aux_moradia) %>% slice(1:n)
  })
  
  output$trib_wit_magis <- renderTable({
    salarios_t %>% 
      group_by(orgao, mes_ano_referencia) %>% 
      mutate(auxilio_medio_magistrados = sum(direitos_pessoais, indenizacoes, direitos_eventuais)/n()) %>% 
      select(orgao, mes_ano_referencia, auxilio_medio_magistrados) %>%
      distinct() %>% 
      arrange(-auxilio_medio_magistrados) %>%
      slice(1:n)
  })
  
  output$top_n_salarios <- renderTable({
    top_n_salarios_historico = data %>% 
      group_by(orgao, mes_ano_referencia) %>% 
      mutate(auxilio_medio_magistrados = sum(direitos_pessoais, indenizacoes, direitos_eventuais)/n()) %>% 
      select(orgao, mes_ano_referencia, auxilio_medio_magistrados) %>%
      distinct() %>% 
      arrange(-auxilio_medio_magistrados)
    
    top_n_salarios_historico[1:3,]
  })
  
  output$magis_mochi <- renderTable({
    data %>% group_by(nome) %>% summarise(aux_diarias = sum(diarias)) %>% 
      arrange(-aux_diarias) %>% slice(1:n)
  })
  
  output$magis_laje <- renderTable({
    data %>% group_by(nome) %>% summarise(aux_moradia = sum(auxilio_moradia)) %>% 
      arrange(-aux_moradia) %>% slice(1:n)
  })
  
  output$magis_estudiosos <- renderTable({
    data %>% group_by(nome) %>% summarise(auxilio_escola = sum(auxilio_pre_escolar)) %>% arrange(-auxilio_escola) %>% slice(1:n)
  })
  
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
      salarios_um = data %>% filter(nome == juiz$nome) %>% group_by(mes_ano_referencia) %>%  summarise(total = sum(rendimento_liquido))
      print(salarios_um)
      plot_ly(data = salarios_um,
              y = ~ total,
              x = ~ mes_ano_referencia,
              type = "scatter",
              source = "temporal",
              mode = "lines")
    } else {
      salarios_um = data %>% group_by(mes_ano_referencia) %>%  summarise(total = sum(rendimento_liquido))
      plot_ly(data = salarios_um,
              y = ~ total,
              x = ~ mes_ano_referencia,
              type = "scatter",
              source = "temporal",
              mode = "lines")
    }
  })
  
  selection = reactive({
    input$sunburst_mouseover
  })
  
  render_sunburst = function(data) {
    custom.message = "function (d) {
    root = d;
    while (root.parent) {
    root = root.parent
    }
    p = (100*d.value/root.value).toPrecision(3);
    if(root.value >= 1000000000) {
    y =   Math.round(root.value/1000000000) + 'B';
    }
    x = d.value
    if(x  >= 1000000000) {
    x =   Math.round(x/1000000000) + 'B';
    }
    
    if(x < 1000000000 && x >= 1000000 ) {
    x = Math.round(x/1000000) + 'M';
    }
    
    
    if(x < 1000000) {
    x =  Math.round(x/1000) + 'K';
    }
    msg = p +' %<br/>'+x+' de '+ y;
    return msg;
    }"
    d <- data.frame(group = c("Salarios","direitos_pessoais-abono_de_permanencia", "direitos_pessoais-subsidio_outra1", "indenizacoes-ajuda_de_custo","indenizacoes-indenizacoes_outros","direitos_eventuais-indenizacao_de_ferias","direitos_eventuais-abono_contitucional_de_1_3_de_ferias","direitos_eventuais-antecipacao_de_ferias","direitos_eventuais-gratificacao_natalina","direitos_eventuais-antecipacao_de_gratificacao_natalina","direitos_eventuais-substituicao","direitos_eventuais-gratificacao_por_exercicio_acumulativo","direitos_eventuais-gratificacao_por_encargo_curso_concurso","direitos_eventuais-pagamento_em_retroativos","direitos_eventuais-jeton", "indenizacoes-auxilio-auxilio_alimentacao", "indenizacoes-auxilio-auxilio_pre_escolar", "indenizacoes-auxilio-auxilio_saude", "indenizacoes-auxilio-auxilio_natalidade", "indenizacoes-auxilio-auxilio_moradia"),
                    value = c(sum(data$subsidio), sum(data$abono_de_permanencia), sum(data$subsidio_outra1),sum(data$ajuda_de_custo),sum(data$indenizacoes_outra1 + data$indenizacoes_outra2 + data$indenizacoes_outra3),sum(data$indenizacao_de_ferias),sum(data$abono_contitucional_de_1_3_de_ferias) ,sum(data$antecipacao_de_ferias) ,sum(data$gratificacao_natalina),sum(data$antecipacao_de_gratificacao_natalina),sum(data$substituicao),sum(data$gratificacao_por_exercicio_cumulativo),sum(data$gratificacao_por_encargo_curso_concurso),sum(data$pagamento_em_retroativos),sum(data$jeton),sum(data$auxilio_alimentacao),sum(data$auxilio_pre_escolar), sum(data$auxilio_saude), sum(data$auxilio_natalidade),sum(data$auxilio_moradia)))
    
    
    sunburst(d,count = TRUE,explanation = custom.message) 
  }
  
  output$fistPlot <- renderSunburst({
    add_shiny(render_sunburst(data))
  })
  
  output$selection = renderText(selection())
  
  output$sumburstPlot  = renderSunburst({
    s <- event_data("plotly_click", source = "juiz")
    s2 <- event_data("plotly_click", source = "temporal")
    if(length(s)) {
      juiz = sumario[s[["pointNumber"]]+1,]
      
      if(length(s2)) {
        salarios_temp = data %>%filter(mes_ano_referencia == s2[["x"]])
      } else {
        salarios_temp = data
      }
      
      magistrado = salarios_temp %>% filter(nome == juiz$nome)
      add_shiny(render_sunburst(magistrado))
    } else {
      NULL
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
      selectInput("orgao", "Selecione um Orgão", choices = data$orgao %>% unique(), selected = NULL)
  })
  
  output$lotacao <- renderUI({
    if(str_length(input$orgao) > 0) {
      selectInput("lotacao", "Selecione uma Lotação", choices = data$lotacao %>%  unique(), selected = NULL)
    } else {
      selectInput("lotacao", "Selecione uma Lotação", c())
    }
  })
  
  output$logo = renderUI({
    tags$img(src = "https://raw.githubusercontent.com/lupanatoga/lupa-na-toga-client/master/www/logo-fodona.jpg", width="100%")
  })
  
  output$cargo <- renderUI({
    if(str_length(input$lotacao) > 0 && str_length(input$orgao) > 0) {
      selectInput("cargo", "Cargo", choices = data$cargo %>%  unique(), selected = NULL)
    } else {
      selectInput("cargo", "Cargo", c())
    }
  })
  
})
