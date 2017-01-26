
library(DT)
library(httr)
library(dplyr)
library(stringr)
library(plotly)
library(visNetwork)
library(shinydashboard)
library(shiny)

source("ui_part.R")

source("server_part.R")

shinyApp(ui, server)