suppressPackageStartupMessages(c(
  library(shiny),
  library(shinyBS),
  library(shinythemes),
  library(rCharts),
  library(timeDate),
  library(forecast),
  library(reshape),
  library(DT),
  library(xts),
  library(dygraphs),
  library(magrittr),
  library(plotly)
))


shinyUI(
ui<-htmlOutput("page")
)