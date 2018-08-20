library(shiny)
library(plotly)
library(sunburstR)
# Define UI

shinyUI(bootstrapPage(
  htmlTemplate("index.html"),
  tags$head(
    includeCSS("www/CSS.css")
  )
))
