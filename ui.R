library(shiny)
library(plotly)
# Define UI

shinyUI(bootstrapPage(
  htmlTemplate("index.html"),
  tags$head(
    includeCSS("www/CSS.css")
  )
))
