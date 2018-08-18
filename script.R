library(plotly)
library(tidyverse)

data = read_csv("salarios-magistrados-2018-02-05.csv")
data[is.na(data)] = 0

plot_ly(data = data %>% select(subsidio, indenizacoes), 
        y = ~ indenizacoes, 
        x = ~ subsidio, 
        name = "Haters",
        type = "scatter",
        hoverinfo = 'text',
        marker = list(size = 12))